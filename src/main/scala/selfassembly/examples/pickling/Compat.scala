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
    val bundle = new PicklerMacros {}
    c.Expr[SPickler[T]](bundle.genQuery[T, SPickler.type](c)) //c.Expr[SPickler[T]](bundle.impl[T](format.tree))
  }

  def UnpicklerMacros_impl[T: c.WeakTypeTag](c: Context)(format: c.Expr[PickleFormat]): c.Expr[Unpickler[T]] = {
    val bundle = new UnpicklerMacros {}
    c.Expr[Unpickler[T]](bundle.genQuery[T, Unpickler.type](c)) //c.Expr[Unpickler[T]](bundle.impl[T])
  }

  def UnpickleMacros_pickleUnpickle[T: c.WeakTypeTag](c: Context): c.Expr[T] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with UnpickleMacros
    c.Expr[T](bundle.pickleUnpickle[T])
  }

  def UnpickleMacros_readerUnpickle[T: c.WeakTypeTag](c: Context): c.Expr[T] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with UnpickleMacros
    c.Expr[T](bundle.readerUnpickle[T])
  }

  def UnpickleMacros_readerUnpickleTopLevel[T: c.WeakTypeTag](c: Context): c.Expr[T] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with UnpickleMacros
    c.Expr[T](bundle.readerUnpickleTopLevel[T])
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
