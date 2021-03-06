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
 * Binary query that handles cycles in object graphs.
 */
trait Query2[R] extends AcyclicQuery2[R] {

  def mkTrees[C <: Context with Singleton](c: C): Trees[C]

  abstract class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) { }

  override def genQuery[T: c.WeakTypeTag, S <: Singleton : c.WeakTypeTag](c: Context): c.Tree = {
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

      val fieldTrees: List[Tree] = {
        tpe match {
          case ExistentialType(quantified, tpe) => c.abort(c.enclosingPosition, "urk!")
          case _ => /* do nothing */
        }
        var isFirst = true
        paramFields.map { sym =>
          val symTp     = sym.typeSignatureIn(tpe)
          val fieldName = sym.name.toString.trim
          val valueTree = trees.fieldValueTree(fieldName, symTp, tpeOfTypeClass)
          val next      = c.Expr[R](q"res")
          val sepTree   =
            if (isFirst) { isFirst = false; q"" }
            else q"combineResult = ${trees.combine(acc, trees.separator).tree}"
          q"""
            $sepTree
            ${trees.preInvoke(symTp)}
            val res: $qresTpe = $valueTree
            combineResult     = ${trees.combine(acc, next).tree}
          """
        }
      }

      val postfixTree  = c.Expr[R](q"postfix")
      val lastCombine  = q"combineResult = ${trees.combine(acc, postfixTree)}"

      q"""
        var combineResult: $qresTpe = ${trees.first(tpe)}
        if (!visited(visitee)) {
          ..$fieldTrees
        }
        val postfix: $qresTpe = ${trees.last(tpe)}
        $lastCombine
        combineResult
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


trait AcyclicQuery2[R] extends Traversal2[R] {

  def mkTrees[C <: Context with Singleton](c: C): Trees[C]

  abstract class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) {
    // these parameters could even be Idents!
    // the library could provide them in local vals before passing them to this user-defined method
    def combine(left: c.Expr[R], right: c.Expr[R]): c.Expr[R]

    def instanceType(elemTpe: c.Type): c.Tree = ???

    def first(tpe: c.Type): c.Expr[R]

    def last(tpe: c.Type): c.Expr[R]

    def separator: c.Expr[R]
  }

  def genQuery[T: c.WeakTypeTag, S <: Singleton : c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val tpe             = weakTypeTag[T].tpe
    val stpe            = weakTypeTag[S].tpe
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

    def theLogic: Tree = {
      val paramFields = trees.paramFieldsOf(tpe)
      val acc = c.Expr[R](q"combineResult")

      val fieldTrees: List[Tree] = {
        tpe match {
          case ExistentialType(quantified, tpe) => c.abort(c.enclosingPosition, "urk!")
          case _ => /* do nothing */
        }
        var isFirst = true
        paramFields.map { sym =>
          val symTp     = sym.typeSignatureIn(tpe)
          val fieldName = sym.name.toString.trim
          val valueTree = trees.fieldValueTree(fieldName, symTp, tpeOfTypeClass)
          val next      = c.Expr[R](q"res")
          val sepTree   =
            if (isFirst) { isFirst = false; q"" }
            else q"combineResult = ${trees.combine(acc, trees.separator)}"

          q"""
            $sepTree
            val res: $qresTpe = $valueTree
            combineResult     = ${trees.combine(acc, next)}
          """
        }
      }

      val postfixTree  = c.Expr[R](q"postfix")
      val lastCombine  = q"combineResult = ${trees.combine(acc, postfixTree)}"

      q"""
        var combineResult: $qresTpe = ${trees.first(tpe)}
        ..$fieldTrees
        val postfix: $qresTpe = ${trees.last(tpe)}
        $lastCombine
        combineResult
      """
    }

    def tree: Tree = tpe match {
      case definitions.NothingTpe =>
        c.abort(c.enclosingPosition, "urk!")
      case _ =>
        theLogic
    }

    trees.instance(tpe, tpeOfTypeClass, paramTypeExact, qresTpe, tree)
  }

}


trait Traversal2[R] {

  def mkTrees[C <: Context with Singleton](c: C): Trees[C]

  abstract class Trees[C <: Context with Singleton](val c: C) {
    import c.universe._

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
      val valueName1 = c.fresh(TermName("value1"))
      val valueName2 = c.fresh(TermName("value2"))
      val innerMostTree = innerMostFieldLogic(q"($valueName1, $valueName2)", tpe, tpeOfTypeClass)
      val getterTree    = getterLogic(name)
      putField(getterTree, innerMostTree, (valueName1, valueName2), name, tpe)
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

    // TODO: we're ignoring getterLogic right now, so maybe the original design can be improved
    def putField(getterLogic: Tree, innerMostFieldLogic: c.Tree, namesOfValues: (c.TermName, c.TermName),
                 fieldNameString: String, fieldTpe: c.Type): c.Tree = {
      val getterTrees: List[c.Tree] =
        (List(q"visitee._1", q"visitee._2") zip List(namesOfValues._1, namesOfValues._2)) map { case (gt, nv) =>
          q"val $nv: $fieldTpe = $gt.${TermName(fieldNameString)}"
        }

      q"""
        ..$getterTrees
        $innerMostFieldLogic
      """
    }

    def names(tpe: c.Type): (String, c.TermName) = {
      val typeString = tpe.toString.split('.').map(_.capitalize).mkString("")
      val typeName   = c.universe.TypeName(typeString)
      (typeString, c.freshName(typeName.toTermName))
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
      q"$fieldValue"

    def project(param: c.Tree): c.Tree =
      q"$param"

    def preInvoke(fieldTpe: c.Type): c.Tree =
      q"{}"
  }
}
