/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 */
package selfassembly.examples.pickling

import scala.language.experimental.macros

import scala.annotation.implicitNotFound
import scala.reflect.macros.blackbox.Context

import selfassembly.{Queryable, Query}

trait SPickler[T] extends Queryable[(T, PBuilder), Unit] {
  def pickle(visitee: (T, PBuilder)): Unit
}

object SPickler extends CorePicklersUnpicklers

trait GenPicklers {
  implicit def genPickler[T](implicit format: PickleFormat): SPickler[T] = macro Compat.PicklerMacros_impl[T]
}

@implicitNotFound(msg = "Cannot generate an unpickler for ${T}. Recompile with -Xlog-implicits for details")
trait Unpickler[T] {
  def unpickle(visitee: (FastTypeTag[_], PReader)): Any
}

trait GenUnpicklers {
  implicit def genUnpickler[T](implicit format: PickleFormat): Unpickler[T] = macro Compat.UnpicklerMacros_impl[T]
  /*def genUnpickler(mirror: Mirror, tag: FastTypeTag[_])(implicit format: PickleFormat, share: refs.Share): Unpickler[_] = {
    // println(s"generating runtime unpickler for ${tag.tpe}") // NOTE: needs to be an explicit println, so that we don't occasionally fallback to runtime in static cases
    //val runtime = new CompiledUnpicklerRuntime(mirror, tag.tpe)
    val runtime = new InterpretedUnpicklerRuntime(mirror, tag)
    runtime.genUnpickler
  }*/
}

object Unpickler extends CorePicklersUnpicklers
