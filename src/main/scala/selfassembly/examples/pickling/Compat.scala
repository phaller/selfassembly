/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 */
package selfassembly.examples.pickling

import language.experimental.macros
import scala.language.higherKinds

import scala.reflect.macros.Context
import scala.reflect.api.{Universe => ApiUniverse}
import scala.reflect.runtime.{universe => ru}

import scala.collection.mutable
import scala.collection.immutable
import scala.collection.generic.CanBuildFrom

// this is only necessary because 2.10.x doesn't support macro bundles
object Compat {
  // provides a source compatibility stub
  implicit class HasPt[A, B](t: (A, B)) {
    def pt: A = t._1
  }

  def PicklerMacros_impl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[SPickler[T]] = {
    //val c0: c.type = c
    val bundle = new /*{ val c: c0.type = c0 } with*/ PicklerMacros {}
    //c.Expr[SPickler[T]](bundle.impl[T](format.tree))
    c.Expr[SPickler[T]](bundle.genQuery[T, SPickler.type](c))
  }

  def CurrentMirrorMacro_impl(c: Context): c.Expr[ru.Mirror] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with CurrentMirrorMacro
    c.Expr[ru.Mirror](bundle.impl)
  }

  def FastTypeTagMacros_impl[T: c.WeakTypeTag](c: Context): c.Expr[FastTypeTag[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with FastTypeTagMacros
    c.Expr[FastTypeTag[T]](bundle.impl[T])
  }

  def FastTypeTagMacros_apply(c: Context)(key: c.Expr[String]): c.Expr[FastTypeTag[t]] forSome { type t } = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with FastTypeTagMacros
    c.Expr(bundle.apply(key.tree))
  }
}
