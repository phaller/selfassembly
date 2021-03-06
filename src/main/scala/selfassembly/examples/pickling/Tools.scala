/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 */
package selfassembly.examples.pickling

//import scala.pickling.internal._

import scala.language.existentials

import scala.reflect.macros.Context
import scala.reflect.api.Universe

import scala.collection.mutable.{Map => MutableMap, ListBuffer => MutableList, WeakHashMap, Set => MutableSet}
import scala.collection.mutable.{Stack => MutableStack, Queue => MutableQueue}

import java.lang.ref.WeakReference

//import HasCompat._

abstract class ShareAnalyzer[U <: Universe](val u: U) {
  import u._
  import definitions._

  val irs = new ir.IRs[u.type](u)
  import irs._

  // FIXME: duplication wrt pickling.`package`, but I don't really fancy abstracting away this path-dependent madness
  implicit class RichTypeFIXME(tpe: Type) {
    def isEffectivelyPrimitive: Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.isEffectivelyPrimitive => true
      case _ => false
    }
  }

  def shareEverything: Boolean
  def shareNothing: Boolean

  // TODO: cache this, because it's not cheap and it's going to be called a lot of times for the same types
  def canCauseLoops(tpe: Type): Boolean = {
    def loop(todo: List[Type], visited: Set[Type]): Boolean = {
      todo match {
        case currTpe :: rest =>
          val currSym = currTpe.typeSymbol.asType
          if (visited(currTpe)) {
            if (tpe <:< currTpe) true  // TODO: make sure this sanely works for polymorphic types
            else loop(rest, visited)
          } else if (/*currTpe.isNotNullable || */currTpe.isEffectivelyPrimitive || currSym == StringClass || currSym.isModuleClass) loop(rest, visited)
          // TODO: extend the traversal logic to support sealed classes
          // when doing that don't forget:
          // 1) sealeds can themselves be extended, so we need to recur
          // 2) the entire sealed hierarchy should be added to todo
          else if (!currSym.isFinal) true // NOTE: returning true here is important for soundness!
          else {
            val more = flattenedClassIR(currTpe).fields.map(_.tpe)
            loop(rest ++ more, visited + currTpe)
          }
        case _ => false
      }
    }
    loop(List(tpe), Set())
  }

  def shouldBotherAboutSharing(tpe: Type): Boolean = {
    if (shareNothing) false
    else if (shareEverything) !tpe.isEffectivelyPrimitive || (tpe.typeSymbol.asType == StringClass)
    else canCauseLoops(tpe)
  }

  def shouldBotherAboutLooping(tpe: Type): Boolean = {
    if (shareNothing) false
    else canCauseLoops(tpe)
  }

  def shouldBotherAboutCleaning(tpe: Type): Boolean = {
    if (shareNothing) false
    else true // TODO: need to be more precise here
  }
}

abstract class Macro { self =>
  val c: Context
  import c.universe._
  import compat._
  import definitions._
  val RefTpe = weakTypeOf[refs.Ref]

  val tools = new selfassembly.Tools[c.type](c)
  import tools._

  val shareAnalyzer = new ShareAnalyzer[c.universe.type](c.universe) {
    def shareEverything = self.shareEverything
    def shareNothing = self.shareNothing
  }

  val irs = new ir.IRs[c.universe.type](c.universe)
  import irs._

  private def innerType(target: Tree, name: String): Type = {
    def fail(msg: String) = c.abort(c.enclosingPosition, s"$msg for ${target} of type ${target.tpe}")
    // val carrier = c.typeCheck(tq"${target.tpe}#${TypeName(name)}", mode = c.TYPEmode, silent = true)
    val carrier = c.typeCheck(q"{ val x: ${target.tpe}#${newTypeName(name)} = ??? }", silent = true)
    carrier match {
      case EmptyTree => fail(s"Couldn't resolve $name")
      case Block(ValDef(_, _, tpt, _) :: _, _) => tpt.tpe.normalize match {
        case tpe if tpe.typeSymbol.isClass => tpe
        case tpe => fail(s"$name resolved as $tpe is invalid")
      }
    }
  }

  // FIXME: duplication wrt pickling.`package`, but I don't really fancy abstracting away this path-dependent madness
  implicit class RichTypeFIXME(tpe: Type) {
    def key: String = {
      tpe.normalize match {
        case ExistentialType(tparams, TypeRef(pre, sym, targs))
        if targs.nonEmpty && targs.forall(targ => tparams.contains(targ.typeSymbol)) =>
          TypeRef(pre, sym, Nil).key
        case TypeRef(pre, sym, targs) if pre.typeSymbol.isModuleClass =>
          sym.fullName +
          (if (sym.isModuleClass) ".type" else "") +
          (if (targs.isEmpty) "" else targs.map(_.key).mkString("[", ",", "]"))
        case _ =>
          tpe.toString
      }
    }
    def canCauseLoops: Boolean = shareAnalyzer.canCauseLoops(tpe)
    def isEffectivelyPrimitive: Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.isEffectivelyPrimitive => true
      case _ => false
    }
  }

  def shouldBotherAboutCleaning(tpe: Type) = shareAnalyzer.shouldBotherAboutCleaning(tpe)
  def shouldBotherAboutSharing(tpe: Type) = shareAnalyzer.shouldBotherAboutSharing(tpe)
  def shouldBotherAboutLooping(tpe: Type) = shareAnalyzer.shouldBotherAboutLooping(tpe)

  def shareEverything = {
    val shareEverything = c.inferImplicitValue(typeOf[refs.ShareEverything]) != EmptyTree
    val shareNothing = c.inferImplicitValue(typeOf[refs.ShareNothing]) != EmptyTree
    if (shareEverything && shareNothing) c.abort(c.enclosingPosition, "inconsistent sharing configuration: both ShareEverything and ShareNothing are in scope")
    shareEverything
  }

  def shareNothing = {
    val shareEverything = c.inferImplicitValue(typeOf[refs.ShareEverything]) != EmptyTree
    val shareNothing = c.inferImplicitValue(typeOf[refs.ShareNothing]) != EmptyTree
    if (shareEverything && shareNothing) c.abort(c.enclosingPosition, "inconsistent sharing configuration: both ShareEverything and ShareNothing are in scope")
    shareNothing
  }

  def pickleFormatType(pickle: Tree): Type = innerType(pickle, "PickleFormatType")

  def compileTimeDispatchees(tpe: Type): List[Type] = tools.compileTimeDispatchees(tpe, rootMirror)

  def syntheticPackageName: String = "scala.pickling.synthetic"
  def syntheticBaseName(tpe: Type): TypeName = {
    val raw = tpe.key.split('.').map(_.capitalize).mkString("")
    val encoded = newTypeName(raw).encoded
    newTypeName(encoded)
  }
  def syntheticBaseQualifiedName(tpe: Type): TypeName = newTypeName(syntheticPackageName + "." + syntheticBaseName(tpe).toString)

  def syntheticPicklerName(tpe: Type): TypeName = syntheticBaseName(tpe) + syntheticPicklerSuffix()
  def syntheticPicklerQualifiedName(tpe: Type): TypeName = syntheticBaseQualifiedName(tpe) + syntheticPicklerSuffix()
  def syntheticPicklerSuffix(): String = "Pickler"

  def syntheticUnpicklerName(tpe: Type): TypeName = syntheticBaseName(tpe) + syntheticUnpicklerSuffix()
  def syntheticUnpicklerQualifiedName(tpe: Type): TypeName = syntheticBaseQualifiedName(tpe) + syntheticUnpicklerSuffix()
  def syntheticUnpicklerSuffix(): String = "Unpickler"

  def syntheticPicklerUnpicklerName(tpe: Type): TypeName = syntheticBaseName(tpe) + syntheticPicklerUnpicklerSuffix()
  def syntheticPicklerUnpicklerQualifiedName(tpe: Type): TypeName = syntheticBaseQualifiedName(tpe) + syntheticPicklerUnpicklerSuffix()
  def syntheticPicklerUnpicklerSuffix(): String = "PicklerUnpickler"

  def preferringAlternativeImplicits(body: => Tree): Tree = {
    import Compat._

    val candidates = c.enclosingImplicits
    val ourPt      = candidates.head.pt

    def debug(msg: Any) = {
      val padding = "  " * (candidates.length - 1)
      // Console.err.println(padding + msg)
    }

    debug("can we enter " + ourPt + "?")
    debug(candidates)

    if ((candidates.size >= 2) && {
      val theirPt = candidates.tail.head.pt
      ourPt =:= theirPt
    }) {
      debug(s"no, because: ourPt = $ourPt, theirPt = ${candidates.tail.head.pt}")
      // c.diverge()
      c.abort(c.enclosingPosition, "stepping aside: repeating itself")
    } else {
      debug(s"not sure, need to explore alternatives")
      c.inferImplicitValue(ourPt, silent = true) match {
        case success if success != EmptyTree =>
          debug(s"no, because there's $success")
          c.abort(c.enclosingPosition, "stepping aside: there are other candidates")
          // c.diverge()
        case _ =>
          debug("yes, there are no obstacles. entering " + ourPt)
          val result = body
          debug("result: " + result)
          result
      }
    }
  }

  private var reflectivePrologueEmitted = false // TODO: come up with something better
  def reflectively(target: String, fir: FieldIR)(body: Tree => Tree): List[Tree] = reflectively(newTermName(target), fir)(body)

  /**
   *  requires: !fir.accessor.isEmpty
   */
  def reflectively(target: TermName, fir: FieldIR)(body: Tree => Tree): List[Tree] = {
    val prologue = {
      if (!reflectivePrologueEmitted) {
        reflectivePrologueEmitted = true
        val initMirror = q"""
          import scala.reflect.runtime.universe._
          val mirror = runtimeMirror(this.getClass.getClassLoader)
          val im = mirror.reflect($target)
        """.asInstanceOf[Block]
        initMirror.stats :+ initMirror.expr
      } else {
        Nil
      }
    }
    val field = fir.field.get
    val ownerSymbol = newTermName(fir.name + "Owner")
    val firSymbol = newTermName(fir.name + "Symbol")
    // TODO: make sure this works for:
    // 1) private[this] fields
    // 2) inherited private[this] fields
    // 3) overridden fields
    val wrappedBody =
      q"""
        val $ownerSymbol = implicitly[selfassembly.examples.pickling.FastTypeTag[${field.owner.asClass.toType.erasure}]].tpe
        val $firSymbol = $ownerSymbol.member(newTermName(${field.name.toString}))
        if ($firSymbol.isTerm) ${body(q"im.reflectField($firSymbol.asTerm)")}
      """.asInstanceOf[Block]
    prologue ++ wrappedBody.stats :+ wrappedBody.expr
  }
}

case class Hints(
  tag: FastTypeTag[_] = null,
  knownSize: Int = -1,
  isStaticallyElidedType: Boolean = false,
  isDynamicallyElidedType: Boolean = false,
  oid: Int = -1) {
  def isElidedType = isStaticallyElidedType || isDynamicallyElidedType
}

trait PickleTools {
  var hints = new Hints()
  var areHintsPinned = false

  def hintTag(tag: FastTypeTag[_]): this.type = { hints = hints.copy(tag = tag); this }
  def hintKnownSize(knownSize: Int): this.type = { hints = hints.copy(knownSize = knownSize); this }
  def hintStaticallyElidedType(): this.type = { hints = hints.copy(isStaticallyElidedType = true); this }
  def hintDynamicallyElidedType(): this.type = { hints = hints.copy(isDynamicallyElidedType = true); this }
  def hintOid(oid: Int): this.type = { hints = hints.copy(oid = oid); this }
  def pinHints(): this.type = { areHintsPinned = true; this }
  def unpinHints(): this.type = { areHintsPinned = false; hints = new Hints(); this }

  def withHints[T](body: Hints => T): T = {
    val hints = this.hints
    if (!areHintsPinned) this.hints = new Hints
    body(hints)
  }
}

trait CurrentMirrorMacro extends Macro {
  import scala.reflect.runtime.{universe => ru}

  def impl: c.Tree = {
    import c.universe._
    c.inferImplicitValue(typeOf[ru.Mirror], silent = true) orElse {
      val cachedMirror = q"selfassembly.examples.pickling.internal.`package`.cachedMirror"
      q"""
        if ($cachedMirror != null) $cachedMirror
        else {
          $cachedMirror = scala.reflect.runtime.currentMirror
          $cachedMirror
        }
      """
    }
  }
}
