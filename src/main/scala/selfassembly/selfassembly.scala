/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 *
 * @author Philipp Haller
 * @author Heather Miller
 */
package selfassembly

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context


/**
 * To handle cycles in a more local way, we require type classes
 * to extend the `Queryable` trait.
 */
trait Queryable[T, R] {
  // used for invocations that are not top-level
  def apply(visitee: T, visited: Set[Any]): R
}


trait Traversal[R] {

  type SContext = Context with Singleton

  def mkTrees[C <: SContext](c: C): Trees[C]

  abstract class Trees[C <: SContext](val c: C) {
    import c.universe._

    def constant[T: c.WeakTypeTag](value: T): c.Expr[T] =
      core.constant(c)(value)

    /**
     * Apply the type class instance `inst` to value `value`.
     */
    def invoke(inst: c.Tree, value: c.Tree): c.Tree =
      q"$inst.apply(${inject(value)}, visited + visitee)"

    /**
     * Apply the type class instance `inst` to value `value`.
     */
    def invokeNotVisited(inst: c.Tree, value: c.Tree): c.Tree =
      q"$inst.apply(${inject(value)}, visited)"

    def instance(tpe: c.Type, tpeOfTypeClass: c.Type, paramTpe: c.Type, qresTpe: c.Type, tree: c.Tree): c.Tree = {
      val (typeString, instanceName) = names(tpe)
      val instType                   = appliedType(tpeOfTypeClass, tpe)
      val methodName                 = tpeOfTypeClass.decls.head.asMethod.name
      q"""
        implicit object $instanceName extends $instType {
          def $methodName(visitee: $paramTpe): $qresTpe = apply(visitee, scala.collection.immutable.Set[Any]())
          def apply(visitee: $paramTpe, visited: scala.collection.immutable.Set[Any]) = $tree
        }
        $instanceName
      """
    }

    def implicitlyTree(tpe: c.Type, tpeOfTypeClass: c.Type): c.Tree =
      q"implicitly[${appliedType(tpeOfTypeClass, tpe)}]"

    def fieldValueTree(name: String, tpe: c.Type, tpeOfTypeClass: c.Type): c.Tree = {
      val valueName     = c.fresh(TermName("value"))
      val innerMostTree = innerMostFieldLogic(q"$valueName", tpe, tpeOfTypeClass)
      val getterTree    = getterLogic(name)
      putField(getterTree, innerMostTree, valueName, name, tpe)
    }

    def getterLogic(fieldName: String): c.Tree = {
      val projected = project(q"visitee")
      q"$projected.${TermName(fieldName)}"
    }

    def innerMostFieldLogic(valuesTree: c.Tree, fieldTpe: c.Type, tpeOfTypeClass: c.Type): c.Tree = {
      val instType = appliedType(tpeOfTypeClass, fieldTpe)
      q"""
        val inst: $instType = ${implicitlyTree(fieldTpe, tpeOfTypeClass)}
        ${invoke(q"inst", q"$valuesTree")}
      """
    }

    def putField(getterLogic: Tree, innerMostFieldLogic: c.Tree, nameOfValue: c.TermName,
                 fieldNameString: String, fieldTpe: c.Type): c.Tree =
      q"""
        val $nameOfValue: $fieldTpe = $getterLogic
        $innerMostFieldLogic
      """

    def names(tpe: c.Type): (String, c.TermName) = {
      val typeString = {
        val raw = tpe/*.key*/.toString.split('.').map(_.capitalize).mkString("")
        newTypeName(raw).encoded
      }
      val typeName = TermName(typeString)
      (typeString, typeName)
    }

    def paramFieldsOf(tpe: c.Type): List[c.Symbol] = {
      val ctor = tpe.decl(termNames.CONSTRUCTOR) match {
        case overloaded: TermSymbol =>
          overloaded.alternatives.head.asMethod
        case primaryCtor: MethodSymbol =>
          primaryCtor
        case NoSymbol =>
          NoSymbol
      }
      val ctorParams =
        if (ctor != NoSymbol) ctor.asMethod.paramLists.flatten.map(_.asTerm) else List()
      val allAccessors =
        tpe.decls.collect { case meth: MethodSymbol if meth.isAccessor || meth.isParamAccessor => meth }
      val (paramAccessors, otherAccessors) =
        allAccessors.partition(_.isParamAccessor)
      // ctor params that are also fields (TODO: verify)
      ctorParams.filter(sym => paramAccessors.find(_.name == sym.name).nonEmpty)
    }

    def inject(fieldValue: c.Tree): c.Tree =
      fieldValue

    def project(param: c.Tree): c.Tree =
      param

    def preInvoke(tpe: c.Type): c.Expr[Unit] =
      reify({})
  }
}


/**
 * Query that handles cycles in object graph.
 */
trait Query[R] extends AcyclicQuery[R] {

  def mkTrees[C <: SContext](c: C) = new Trees(c)

  class Trees[C <: SContext](override val c: C) extends super.Trees(c) { }

  override def genQuery[T:c.WeakTypeTag, S <: Singleton : c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    import definitions.NullTpe

    val tpe             = weakTypeOf[T]
    val stpe            = weakTypeOf[S]
    val typeClassClass  = stpe.typeSymbol.asClass.companion.asType.asClass
    val tpeOfTypeClass  = typeClassClass.toTypeConstructor
    if (tpeOfTypeClass.decls.size > 1) c.abort(c.enclosingPosition, "trait must not have more than a single abstract method")
    val typeClassMethod = tpeOfTypeClass.decls.head.asMethod
    val qresTpe         = typeClassMethod.returnType

    val paramSymbol     = typeClassMethod.paramLists.head.head
    val instType        = appliedType(tpeOfTypeClass, tpe)
    val paramTypeRaw    = paramSymbol.typeSignature
    val paramTypeExact  = paramTypeRaw.asSeenFrom(instType, typeClassClass)

    val trees = mkTrees[c.type](c)
    val tools = new Tools[c.type](c)

    def genDispatchLogic: Option[c.Tree] = {
      val sym = tpe.typeSymbol
      val projected = trees.project(q"visitee")

      def nonFinalDispatch = {
        val nullDispatch =
          CaseDef(Literal(Constant(null)), EmptyTree, trees.invokeNotVisited(trees.implicitlyTree(NullTpe, tpeOfTypeClass), q"null"))
        val compileTimeDispatch =
          tools.compileTimeDispatchees(tpe, rootMirror) filter (_ != NullTpe) map (subtpe =>
            CaseDef(Bind(newTermName("clazz"), Ident(nme.WILDCARD)), q"clazz == classOf[$subtpe]",
              trees.invokeNotVisited(trees.implicitlyTree(subtpe, tpeOfTypeClass), q"$projected.asInstanceOf[$subtpe]")
            )
          )
        val registryName = c.fresh(TermName("registry"))
        val lookupName = c.fresh(TermName("lookup"))
        val typeOfInstance = appliedType(tpeOfTypeClass, tpe)
        val castedInstanceTree = q"$lookupName.asInstanceOf[$typeOfInstance]"
        val invocationTree = trees.invokeNotVisited(castedInstanceTree, projected)

        val registryLookup = q"""
          val $registryName = implicitly[selfassembly.Registry[$tpeOfTypeClass]]
          val $lookupName = $registryName.get(clazz)
          $invocationTree
        """
        val runtimeDispatch =
          CaseDef(Ident(nme.WILDCARD), EmptyTree, registryLookup)

        q"""
          val clazz = if ($projected != null) $projected.getClass else null
          ${Match(q"clazz", nullDispatch +: compileTimeDispatch :+ runtimeDispatch)}
        """
      }

      if (sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isEffectivelyFinal || sym.asClass.isCaseClass) None
      //TODO: check that the class is sealed abstract
      else Some(nonFinalDispatch)
    }

    def theLogic: c.Expr[R] = {
      tpe match {
        case ExistentialType(quantified, tpe) => c.abort(c.enclosingPosition, "urk!")
        case _ => /* do nothing */
      }

      val visitedExpr = c.Expr[Boolean](q"visited(visitee)") // TRUSTED

      def fieldsExpr(begin: c.Expr[R], sep: c.Expr[R]): c.Expr[R] = {
        def fieldValueExpr(sym: c.Symbol): c.Expr[R] = {
          val symTp     = sym.typeSignatureIn(tpe)
          val fieldName = sym.name.toString.trim
          c.Expr[R](trees.fieldValueTree(fieldName, symTp, tpeOfTypeClass)) // TRUSTED
        }

        val paramFields = trees.paramFieldsOf(tpe)
        if (paramFields.size == 0) begin
        else {
          val startExpr =
            trees.combine(begin, fieldValueExpr(paramFields.head))
          if (paramFields.size == 1) startExpr
          else
            paramFields.tail.foldLeft(startExpr) { (acc, sym) =>
              val withSep = trees.combine(acc, sep)
              trees.combine(withSep, fieldValueExpr(sym))
            }
        }
      }

      val (first, separator, last) =
        trees.delimit(tpe)
      val preInvoke =
        trees.preInvoke(tpe)
      val combinedFields =
        trees.combine(fieldsExpr(first, separator), last)
      val noFields =
        trees.combine(first, last)

      reify {
        preInvoke.splice
        if (!visitedExpr.splice)
          combinedFields.splice
        else
          noFields.splice
      }
    }

    def tree: Tree = tpe match {
      case definitions.NothingTpe =>
        c.abort(c.enclosingPosition, "urk!")
      case _ =>
        /* tpe might be an abstract class (handle only sealed classes for now)
           Thus, we first do a dynamic dispatch to find out which concrete instance to dispatch to.
        */
        genDispatchLogic match {
          case Some(invokeInstance) => invokeInstance
          case None => theLogic.tree
        }
    }

    trees.instance(tpe, tpeOfTypeClass, paramTypeExact, qresTpe, tree)
  }

}


trait Result[R] {
  // needs usecase for API docs
  def plus(other: Result[String]): Result[String] = {
    val thisRes  = this.asInstanceOf[InternalResult[String]]
    val otherRes = other.asInstanceOf[InternalResult[String] { type Ctx = thisRes.Ctx }]

    val resExpr = thisRes.c.universe.reify { thisRes.expr.splice + otherRes.expr.splice }
    new InternalResult[String] {
      type Ctx = thisRes.Ctx
      val  c   = thisRes.c
      val  expr: c.Expr[String] = resExpr.asInstanceOf[c.universe.Expr[String]]
    }
  }
}

abstract class InternalResult[R] extends Result[R] {
  type Ctx <: Context with Singleton
  protected[selfassembly] val c: Ctx
  protected[selfassembly] val expr: c.Expr[R]
}

trait AcyclicQuery[R] extends Traversal[R] {
  outer =>

  def combine(left: Result[R], right: Result[R]): Result[R] = ???

  def delimit(rep: TypeRep): (R, R, R) = ???

  def mkTrees[C <: Context with Singleton](c: C): Trees[C]

  abstract class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) {
    self =>

    import c.universe._

    // these parameters could even be Idents!
    // the library could provide them in local vals before passing them to this user-defined method
    def combine(left: c.Expr[R], right: c.Expr[R]): c.Expr[R] = {
      val leftRes  = new InternalResult[R] {
        type Ctx   = C
        val  c: self.c.type = self.c
        val  expr  = left
      }
      val rightRes = new InternalResult[R] {
        type Ctx   = C
        val  c: self.c.type = self.c
        val  expr  = right
      }
      val res = outer.combine(leftRes, rightRes)
      res.asInstanceOf[InternalResult[R]].expr.asInstanceOf[c.Expr[R]]
    }

    def instanceType(elemTpe: c.Type): c.Tree = ???

    def first(tpe: c.Type): c.Expr[R] =
      delimit(tpe)._1

    def last(tpe: c.Type): c.Expr[R] =
      delimit(tpe)._3

    def separator(tpe: c.Type): c.Expr[R] =
      delimit(tpe)._2

    def delimit(tpe: c.Type): (c.Expr[R], c.Expr[R], c.Expr[R]) = {
      val (typeRep, rem) = TypeRep.parse(tpe.toString)
      val (res1, res2, res3) = outer.delimit(typeRep)
      (c.Expr[R](Literal(Constant(res1))), c.Expr[R](Literal(Constant(res2))), c.Expr[R](Literal(Constant(res3))))
    }

    def compose(tpe: c.Type, acc: c.Expr[R], separator: c.Expr[R], fieldValues: List[c.Expr[R]]): c.Expr[R] = {
      var isFirst = true
      val combining = fieldValues map { fieldValue =>
        val sepTree =
          if (isFirst) { isFirst = false; q"" }
          else q"$acc = ${combine(acc, separator)}"
        q"""
          $sepTree
          $acc = ${combine(acc, fieldValue)}
        """
      }
      c.Expr[R](q"""
        ..$combining
        $acc
      """)
    }
  }

  def genQuery[T: c.WeakTypeTag, S <: Singleton : c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    import definitions.NullTpe

    val tpe             = weakTypeOf[T]
    val stpe            = weakTypeOf[S]
    val typeClassClass  = stpe.typeSymbol.asClass.companion.asType.asClass
    val tpeOfTypeClass  = typeClassClass.toTypeConstructor
    if (tpeOfTypeClass.decls.size > 1) c.abort(c.enclosingPosition, "trait must not have more than a single abstract method")
    val typeClassMethod = tpeOfTypeClass.decls.head.asMethod
    val qresTpe         = typeClassMethod.returnType

    val paramSymbol     = typeClassMethod.paramLists.head.head
    val instType        = appliedType(tpeOfTypeClass, tpe)
    val paramTypeRaw    = paramSymbol.typeSignature
    val paramTypeExact  = paramTypeRaw.asSeenFrom(instType, typeClassClass)

    val trees = mkTrees[c.type](c)
    val tools = new Tools[c.type](c)

    def genDispatchLogic: Option[c.Tree] = {
      val sym = tpe.typeSymbol

      def nonFinalDispatch = {
        val nullDispatch =
          CaseDef(Literal(Constant(null)), EmptyTree, trees.invokeNotVisited(trees.implicitlyTree(NullTpe, tpeOfTypeClass), q"null"))
        val compileTimeDispatch =
          tools.compileTimeDispatchees(tpe, rootMirror) filter (_ != NullTpe) map (subtpe =>
            CaseDef(Bind(newTermName("clazz"), Ident(nme.WILDCARD)), q"clazz == classOf[$subtpe]",
              trees.invokeNotVisited(trees.implicitlyTree(subtpe, tpeOfTypeClass), q"visitee.asInstanceOf[$subtpe]")
            )
          )
        val registryName = c.fresh(TermName("registry"))
        val lookupName = c.fresh(TermName("lookup"))
        val typeOfInstance = appliedType(tpeOfTypeClass, tpe)
        val castedInstanceTree = q"$lookupName.asInstanceOf[$typeOfInstance]"
        val invocationTree = trees.invokeNotVisited(castedInstanceTree, q"visitee")

        val registryLookup = q"""
          val $registryName = implicitly[selfassembly.Registry[$tpeOfTypeClass]]
          val $lookupName = $registryName.get(clazz)
          $invocationTree
        """
        val runtimeDispatch =
          CaseDef(Ident(nme.WILDCARD), EmptyTree, registryLookup)

        q"""
          val clazz = if (visitee != null) visitee.getClass else null
          ${Match(q"clazz", nullDispatch +: compileTimeDispatch :+ runtimeDispatch)}
        """
      }

      if (sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isEffectivelyFinal || sym.asClass.isCaseClass) None
      //TODO: check that the class is sealed abstract
      else Some(nonFinalDispatch)
    }

    def theLogic: Tree = {
      val paramFields = trees.paramFieldsOf(tpe)
      val acc = c.Expr[R](q"combineResult")
      val (first, separator, last) = trees.delimit(tpe)

      val fieldTrees: List[c.Expr[R]] = {
        tpe match {
          case ExistentialType(quantified, tpe) => c.abort(c.enclosingPosition, "urk!")
          case _ => /* do nothing */
        }
        var isFirst = true
        paramFields.map { sym =>
          val symTp     = sym.typeSignatureIn(tpe)
          val fieldName = sym.name.toString.trim
          val valueTree = trees.fieldValueTree(fieldName, symTp, tpeOfTypeClass)
          val preInvoke = trees.preInvoke(symTp)
          c.Expr[R](q"""
            $preInvoke
            $valueTree
          """)
        }
      }

      q"""
        var combineResult: $qresTpe = $first
        ${trees.compose(tpe, acc, separator, fieldTrees)}
        ${trees.combine(acc, last)}
      """
    }

    def tree: Tree = tpe match {
      case definitions.NothingTpe =>
        c.abort(c.enclosingPosition, "urk!")
      case _ =>
        /* tpe might be an abstract class (handle only sealed classes for now)
           Thus, we first do a dynamic dispatch to find out which concrete instance to dispatch to.
        */
        genDispatchLogic match {
          case Some(invokeInstance) => invokeInstance
          case None => theLogic
        }
    }

    trees.instance(tpe, tpeOfTypeClass, paramTypeExact, qresTpe, tree)
  }

}

trait Transform extends Traversal[Any] {

  def genTransform[T: c.WeakTypeTag, S <: Singleton : c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val tpe            = weakTypeTag[T].tpe
    val stpe           = weakTypeTag[S].tpe
    val typeClassClass = stpe.typeSymbol.asClass.companion.asType.asClass
    val tpeOfTypeClass = typeClassClass.toTypeConstructor
    if (tpeOfTypeClass.decls.size > 1) c.abort(c.enclosingPosition, "trait must not have more than a single abstract method")

    val typeClassMethod = tpeOfTypeClass.decls.head.asMethod
    val paramSymbol     = typeClassMethod.paramLists.head.head
    val instType        = appliedType(tpeOfTypeClass, tpe)
    val paramTypeRaw    = paramSymbol.typeSignature
    val paramTypeExact  = paramTypeRaw.asSeenFrom(instType, typeClassClass)

    val trees = mkTrees[c.type](c)

    def theLogic: Tree = {
      val paramFields = trees.paramFieldsOf(tpe)

      val fieldTrees: List[Tree] = {
        tpe match {
          case ExistentialType(quantified, tpe) => c.abort(c.enclosingPosition, "urk!")
          case _ => /* do nothing */
        }
        paramFields.map { sym => // don't call combine any more
          val symTp      = sym.typeSignatureIn(tpe)
          val fieldName  = sym.name.toString.trim
          trees.fieldValueTree(fieldName, symTp, tpeOfTypeClass)
        }
      }

      val instantiationLogic =
        q"scala.concurrent.util.Unsafe.instance.allocateInstance(classOf[$tpe]).asInstanceOf[$tpe]"

      val instance = TermName(tpe.typeSymbol.name + "Instance")

      val initPendingFields = (paramFields zip fieldTrees).map {
        case (sym, fldTree) =>
          val fieldName = sym.name.toString.trim
          q"$instance.${TermName(fieldName)} = $fldTree".asInstanceOf[Tree]
      }

      q"""
        val $instance = $instantiationLogic
        ..$initPendingFields
        $instance
      """
    }

    def tree: Tree = tpe match {
      case definitions.NothingTpe =>
        c.abort(c.enclosingPosition, "urk!")
      case _ =>
        theLogic
    }

    trees.instance(tpe, tpeOfTypeClass, paramTypeExact, tpe, tree)
  }

}

trait Transform2 extends Traversal[Any] {

  def genTransform[T: c.WeakTypeTag, S <: Singleton : c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val tpe            = weakTypeTag[T].tpe
    val stpe           = weakTypeTag[S].tpe
    val typeClassClass = stpe.typeSymbol.asClass.companion.asType.asClass
    val tpeOfTypeClass = typeClassClass.toTypeConstructor
    if (tpeOfTypeClass.decls.size > 1) c.abort(c.enclosingPosition, "trait must not have more than a single abstract method")

    val typeClassMethod = tpeOfTypeClass.decls.head.asMethod
    val paramSymbol     = typeClassMethod.paramLists.head.head
    val instType        = appliedType(tpeOfTypeClass, tpe)
    val paramTypeRaw    = paramSymbol.typeSignature
    val paramTypeExact  = paramTypeRaw.asSeenFrom(instType, typeClassClass)

    val trees = mkTrees[c.type](c)

    def theLogic: Tree = {
      val paramFields = trees.paramFieldsOf(tpe)

      val fieldTrees: List[Tree] = {
        tpe match {
          case ExistentialType(quantified, tpe) => c.abort(c.enclosingPosition, "urk!")
          case _ => /* do nothing */
        }
        paramFields.map { sym => // don't call combine any more
          val symTp      = sym.typeSignatureIn(tpe)
          val fieldName  = sym.name.toString.trim
          trees.fieldValueTree(fieldName, symTp, tpeOfTypeClass)
        }
      }

      val instantiationLogic =
        q"scala.concurrent.util.Unsafe.instance.allocateInstance(classOf[$tpe]).asInstanceOf[$tpe]"

      val instance = TermName(tpe.typeSymbol.name + "Instance")

      val initPendingFields = (paramFields zip fieldTrees).map {
        case (sym, fldTree) =>
          val fieldName = sym.name.toString.trim
          q"$instance.${TermName(fieldName)} = $fldTree".asInstanceOf[Tree]
      }

      q"""
        val $instance = $instantiationLogic
        ..$initPendingFields
        $instance
      """
    }

    def tree: Tree = tpe match {
      case definitions.NothingTpe =>
        c.abort(c.enclosingPosition, "urk!")
      case _ =>
        theLogic
    }

    trees.instance(tpe, tpeOfTypeClass, paramTypeExact, tpe, tree)
  }

}


trait Property[R] extends AcyclicQuery[R] {
  def mkTrees[C <: Context with Singleton](c: C): Trees[C]

  abstract class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) {
    import c.universe._

    def check(tpe: c.Type): Unit

    override def combine(left: c.Expr[R], right: c.Expr[R]): c.Expr[R] = {
      c.Expr[R](q"$left ; $right")
    }

    override def invoke(inst: c.Tree, value: c.Tree): c.Tree =
      q"{}"

    override def delimit(tpe: c.Type): (c.Expr[R], c.Expr[R], c.Expr[R]) = {
      check(tpe)
      val empty = c.Expr(q"{}")
      (empty, empty, empty)
    }
  }
}
