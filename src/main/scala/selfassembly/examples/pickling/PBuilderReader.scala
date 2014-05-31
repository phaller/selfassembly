/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 */
package selfassembly.examples.pickling

trait Hintable {
  def hintTag(tag: FastTypeTag[_]): this.type
  def hintKnownSize(knownSize: Int): this.type
  def hintStaticallyElidedType(): this.type
  def hintDynamicallyElidedType(): this.type
  def hintOid(id: Int): this.type
  def pinHints(): this.type
  def unpinHints(): this.type
}

trait PBuilder extends Hintable {
  def beginEntry(picklee: Any): PBuilder
  def putField(name: String, pickler: PBuilder => Unit): PBuilder
  def endEntry(): Unit
  def beginCollection(length: Int): PBuilder
  def putElement(pickler: PBuilder => Unit): PBuilder
  def endCollection(): Unit
  def result(): Pickle
}
