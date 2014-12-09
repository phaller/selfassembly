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


/** The `Show` type class.
 */
trait Show[T] extends Queryable[T, String] {
  def show(x: T): String
}

object Show extends Query[String] {

  // TODO: ideally without `override`
  override def combine(left: Result[String], right: Result[String]) =
    left.plus(right)

  // TODO: ideally without `override`
  override def delimit(rep: TypeRep) =
    (rep.toString + "(", ", ", ")")

  implicit def generate[T]: Show[T] =
    macro genQuery[T, this.type]

  implicit val intHasShow: Show[Int] = new Show[Int] {
    def show(x: Int): String = "" + x
    def apply(visitee: Int, visited: Set[Any]): String = show(visitee)
  }

  implicit val stringHasShow: Show[String] = new Show[String] {
    def show(x: String): String = x
    def apply(visitee: String, visited: Set[Any]): String = show(visitee)
  }
}
