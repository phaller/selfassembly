/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 *
 * @author Philipp Haller
 * @author Heather Miller
 */
package selfassembly
package examples

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context


trait Scale[T] extends Queryable[T, T] {
  def scale(x: T): T
}

/* This is a transformation. It multiplies all integers by 10. All other objects
 * are copied unchanged.
 */
object Scale extends Transform {

  implicit def generate[T]: Scale[T] = macro genTransform[T, this.type]

  implicit val intHasScale: Scale[Int] = new Scale[Int] {
    def scale(x: Int): Int = x * 10
    def apply(visitee: Int, visited: Set[Any]): Int = scale(visitee)
  }

  implicit val stringHasScale: Scale[String] = new Scale[String] {
    def scale(x: String): String = x
    def apply(visitee: String, visited: Set[Any]): String = scale(visitee)
  }
}
