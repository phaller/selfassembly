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


/** The `Eq` type class.
 */
trait Eq[T] extends Queryable[(T, T), Boolean] {
  def eq(visitee: (T, T)): Boolean
}

object Eq extends Query2[Boolean] {
  def mkTrees[C <: Context with Singleton](c: C): Trees[C] =
    new Trees(c)

  class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) {
    import c.universe._

    def combine(left: c.Expr[Boolean], right: c.Expr[Boolean]): c.Expr[Boolean] =
      c.Expr(q"$left && $right")

    def first(tpe: c.Type): c.Expr[Boolean] =
      reify(true)

    def last(tpe: c.Type): c.Expr[Boolean] =
      reify(true)

    def separator: c.Expr[Boolean] =
      reify(true)
  }

  implicit def generate[T]: Eq[T] = macro genQuery[T, this.type]

  implicit val intHasEq: Eq[Int] = new Eq[Int] {
    def eq(visitee: (Int, Int)): Boolean = visitee._1 == visitee._2
    def apply(visitee: (Int, Int), visited: Set[Any]): Boolean = eq(visitee)
  }

  implicit val stringHasEq: Eq[String] = new Eq[String] {
    def eq(visitee: (String, String)): Boolean = visitee._1 == visitee._2
    def apply(visitee: (String, String), visited: Set[Any]): Boolean = eq(visitee)
  }
}
