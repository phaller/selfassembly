/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 */
package selfassembly.examples.pickling

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import selfassembly.{Queryable, Query}

trait SPickler[T] extends Queryable[(T, PBuilder), Unit] {
  def pickle(visitee: (T, PBuilder)): Unit
}

object SPickler extends CorePicklersUnpicklers

trait GenPicklers {
  implicit def genPickler[T](implicit format: PickleFormat): SPickler[T] = macro Compat.PicklerMacros_impl[T]
}
