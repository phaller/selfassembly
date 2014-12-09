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


trait ToString[T] extends Queryable[T, String] {
  def mkString(x: T): String
}

/*
Given this definition of the ToString[T] trait, we can already provide an implementation
of the generic toString function:
*/

object example {

  def toString[T](x: T)(implicit inst: ToString[T]): String =
    inst.mkString(x)

}

object ToString extends AcyclicQuery[String] {

  def mkTrees[C <: Context with Singleton](c: C): Trees[C] =
    new Trees(c)

  class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) {
    import c.universe._

    override def combine(left: c.Expr[String], right: c.Expr[String]): c.Expr[String] = {
      c.Expr[String](q"$left + $right")
    }

    override def delimit(tpe: c.Type): (c.Expr[String], c.Expr[String], c.Expr[String]) = {
      val start = tpe.toString.split('.').map(_.capitalize).mkString("") + "("
      (c.Expr(q"$start"), reify(", "), reify(")"))
    }
  }

  implicit def generate[T]: ToString[T] = macro genQuery[T, this.type]

  implicit val intToString: ToString[Int] = new ToString[Int] {
    def mkString(x: Int): String = "" + x
    def apply(visitee: Int, visited: Set[Any]) = mkString(visitee)
  }

  implicit val stringToString: ToString[String] = new ToString[String] {
    def mkString(x: String): String = x
    def apply(visitee: String, visited: Set[Any]) = mkString(visitee)
  }
}
