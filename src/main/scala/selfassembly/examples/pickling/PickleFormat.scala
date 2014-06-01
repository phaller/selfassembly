/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 */
package selfassembly.examples.pickling

import scala.language.experimental.macros

import scala.reflect.runtime.universe.Mirror

trait Pickle {
  type ValueType
  val value: ValueType

  type PickleFormatType <: PickleFormat
  def unpickle[T] = macro Compat.UnpickleMacros_pickleUnpickle[T]
}

trait PickleFormat {
  type PickleType <: Pickle
  type OutputType
  def createBuilder(): PBuilder
  def createBuilder(out: OutputType): PBuilder
  def createReader(pickle: PickleType, mirror: Mirror): PReader
}
