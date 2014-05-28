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
  def mkTrees[C <: Context with Singleton](c: C): Trees[C] =
    new Trees(c)

  class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) {
    import c.universe._

    def combine(left: c.Expr[String], right: c.Expr[String]): c.Expr[String] =
      c.Expr(q"$left + $right")

    def first(tpe: c.Type): c.Expr[String] = {
      val typeString = tpe.typeSymbol.name.toString
      c.Expr(q"""
        $typeString + "("
      """)
    }

    def last(tpe: c.Type): c.Expr[String] =
      reify(")")

    def separator: c.Expr[String] =
      reify(", ")
  }

  implicit def generate[T]: Show[T] = macro genQuery[T, this.type]

  implicit val intHasShow: Show[Int] = new Show[Int] {
    def show(x: Int): String = "" + x
    def apply(visitee: Int, visited: Set[Any]): String = show(visitee)
  }

  implicit val stringHasShow: Show[String] = new Show[String] {
    def show(x: String): String = x
    def apply(visitee: String, visited: Set[Any]): String = show(visitee)
  }
}
