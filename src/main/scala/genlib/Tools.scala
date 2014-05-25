/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 */
package genlib

import scala.reflect.macros.blackbox.Context
import scala.collection.mutable.{Map => MutableMap, ListBuffer => MutableList, WeakHashMap, Set => MutableSet}
import java.lang.ref.WeakReference


object Tools {
  private val subclassCaches = new WeakHashMap[AnyRef, WeakReference[AnyRef]]()

  private object SomeRef {
    def unapply[T](optRef: Option[WeakReference[T]]): Option[T] =
      if (optRef.nonEmpty) {
        val result = optRef.get.get
        if (result != null) Some(result) else None
      } else None
  }

  def subclassCache(key: AnyRef, valueThunk: => AnyRef): AnyRef = {
    subclassCaches get key match {
      case SomeRef(value) =>
        value
      case _ =>
        val value = valueThunk
        subclassCaches(key) = new WeakReference(value)
        value
    }
  }

  val generatedNames = MutableSet[Any]()
}

class Tools[C <: Context with Singleton](val c: C) {
  import c.universe._
  import compat._
  import definitions._

  def blackList(sym: Symbol) = sym == AnyClass || sym == AnyRefClass || sym == AnyValClass || sym == ObjectClass

  def isRelevantSubclass(baseSym: Symbol, subSym: Symbol) = {
    !blackList(baseSym) && !blackList(subSym) && subSym.isClass && {
      val subClass = subSym.asClass
      subClass.baseClasses.contains(baseSym) && !subClass.isAbstractClass && !subClass.isTrait
    }
  }

  def compileTimeDispatchees(tpe: Type, mirror: Mirror): List[Type] = {
    // TODO: why do we need nullTpe?
    val nullTpe = if (tpe.baseClasses.contains(ObjectClass)) List(NullTpe) else Nil
    val subtypes = allStaticallyKnownConcreteSubclasses(tpe, mirror).filter(subtpe => subtpe.typeSymbol != tpe.typeSymbol)
    val selfTpe = if (isRelevantSubclass(tpe.typeSymbol, tpe.typeSymbol)) List(tpe) else Nil
    val result = nullTpe ++ subtypes ++ selfTpe
    // println(s"$tpe => $result")
    result
  }

  def allStaticallyKnownConcreteSubclasses(tpe: Type, mirror: Mirror): List[Type] = {
    // TODO: so far the search is a bit dumb
    // given `class C[T]; class D extends C[Int]` and `tpe = C[String]`, it will return <symbol of D>
    // TODO: on a more elaborate note
    // given `class C; class D[T] extends C` we of course cannot return the infinite number of `D[X]` types
    // but what we can probably do is to additionally look up custom picklers/unpicklers of for specific `D[X]`
    val baseSym = tpe.typeSymbol.asType
    val baseTargs = tpe match { case TypeRef(_, _, args) => args; case _ => Nil }

    def sourcepathScan(): List[Symbol] = {
      val subclasses = MutableList[Symbol]()
      def analyze(sym: Symbol) = if (isRelevantSubclass(baseSym, sym)) subclasses += sym
      def loop(tree: Tree): Unit = tree match {
        // NOTE: only looking for top-level classes!
        case PackageDef(_, stats) => stats.foreach(loop)
        case cdef: ClassDef => analyze(cdef.symbol)
        case mdef: ModuleDef => analyze(mdef.symbol.asModule.moduleClass)
        case _ => // do nothing
      }
      c.enclosingRun.units.map(_.body).foreach(loop)
      subclasses.toList
    }

    def sealedHierarchyScan(): List[Symbol] = {
      var hierarchyIsSealed = true
      def loop(sym: ClassSymbol): List[ClassSymbol] = {
        sym +: {
          val initialize = sym.typeSignature
          if (sym.isFinal || sym.isModuleClass) {
            Nil
          } else if (sym.isSealed) {
            val syms: List[ClassSymbol] =
              sym.knownDirectSubclasses.toList.map {
                case csym: ClassSymbol => csym
                case msym: ModuleSymbol => msym.moduleClass.asClass
                case osym => throw new Exception(s"unexpected known direct subclass: $osym <: $sym")
              }.flatMap(loop)
            syms
          } else {
            hierarchyIsSealed = false
            Nil
          }
        }
      }
      if (baseSym.isClass) {
        val sealedHierarchy = loop(baseSym.asClass)
        if (hierarchyIsSealed) sealedHierarchy
        else sealedHierarchy ++ sourcepathScan()
      } else sourcepathScan()
    }

    if (baseSym.isFinal || baseSym.isModuleClass) Nil // FIXME: http://groups.google.com/group/scala-internals/browse_thread/thread/e2b786120b6d118d
    else if (blackList(baseSym)) Nil
    else {
      var unsorted = {
        if (baseSym.isClass && baseSym.asClass.isSealed) sealedHierarchyScan()
        else sourcepathScan()
      }
      // NOTE: need to order the list: children first, parents last
      // otherwise pattern match which uses this list might work funnily
      val subSyms = unsorted.distinct.sortWith((c1, c2) => c1.asClass.baseClasses.contains(c2))
      val subTpes = subSyms.map(_.asClass).map(subSym => {
        def tparamNames(sym: TypeSymbol) = sym.typeParams.map(_.name.toString)
        // val tparamsMatch = subSym.typeParams.nonEmpty && tparamNames(baseSym) == tparamNames(subSym)
        val tparamsMatch = subSym.typeParams.nonEmpty && tparamNames(baseSym).length == tparamNames(subSym).length
        val targsAreConcrete = baseTargs.nonEmpty && baseTargs.forall(_.typeSymbol.isClass)
        // NOTE: this is an extremely naïve heuristics
        // see http://groups.google.com/group/scala-internals/browse_thread/thread/3a43a6364b97b521 for more information
        if (tparamsMatch && targsAreConcrete) appliedType(subSym.toTypeConstructor, baseTargs)
        else existentialAbstraction(subSym.typeParams, subSym.toType)
      })
      subTpes
    }
  }
}
