/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 *
 * @author Philipp Haller
 * @author Heather Miller
 */
package genlib
package examples

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context


trait ToString[T] {
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

object ToString extends Query {

  def mkTrees[C <: Context with Singleton](c: C): Trees[C] =
    new Trees(c)

  class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) {
    def combine(left: c.Tree, right: c.Tree): c.Tree = {
      import c.universe._
      q"$left + $right"
    }

    // Note: this can be automated based on the type of the enclosing singleton object
    /*def instanceType(c: Context)(elemTpe: c.Type): c.Tree = {
      import c.universe._
      tq"ToString[$elemTpe]"
    }*/

    /**
     * Apply the type class instance `inst` to value `value`.
     */
    def invoke(inst: c.Tree, value: c.Tree): c.Tree = {
      import c.universe._
      q"$inst.mkString($value)"
    }

    def first(tpe: c.Type): c.Tree = {
      import c.universe._
      val typeString = tpe.toString.split('.').map(_.capitalize).mkString("")
      q"""
        $typeString + "("
      """
    }

    def last(tpe: c.Type): c.Tree = {
      import c.universe._
      q"""
        ")"
      """
    }

    def separator: c.Tree = {
      import c.universe._
      q"""
        ", "
      """
    }
  }

  implicit def generate[T]: ToString[T] = macro genQuery[T, this.type]

  implicit val intToString: ToString[Int] = new ToString[Int] {
    def mkString(x: Int): String = "" + x
  }

  implicit val stringToString: ToString[String] = new ToString[String] {
    def mkString(x: String): String = x
  }
}


trait Show[T] {
  def doIt(x: T): String
}

object Show extends CyclicQuery {
  def mkTrees[C <: Context with Singleton](c: C): Trees[C] =
    new Trees(c)

  class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) {
    def combine(left: c.Tree, right: c.Tree): c.Tree = {
      import c.universe._
      q"$left + $right"
    }

    def invoke(inst: c.Tree, value: c.Tree): c.Tree = {
      import c.universe._
      q"$inst.doIt($value)"
    }

    def first(tpe: c.Type): c.Tree = {
      import c.universe._
      val typeString = tpe.toString.split('.').map(_.capitalize).mkString("")
      q"""
        $typeString + "("
      """
    }

    def last(tpe: c.Type): c.Tree = {
      import c.universe._
      q"""
        ")"
      """
    }

    def separator: c.Tree = {
      import c.universe._
      q"""
        ", "
      """
    }
  }

  implicit def generate[T]: Show[T] = macro genQuery[T, this.type]

  implicit val intHasShow: Show[Int] = new Show[Int] {
    def doIt(x: Int): String = "" + x
  }

  implicit val stringHasShow: Show[String] = new Show[String] {
    def doIt(x: String): String = x
  }
}


trait Immutable[T] {
  def noop(x: T): Unit
}

object Immutable extends Property {
  def mkTrees[C <: Context with Singleton](c: C): Trees[C] =
    new Trees(c)

  class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) {
    def check(tpe: c.Type): Unit = {
      import c.universe._
      // if tpe has var, abort
      val allAccessors = tpe.decls.collect { case meth: MethodSymbol if meth.isAccessor || meth.isParamAccessor => meth }
      val varGetters   = allAccessors.collect { case meth if meth.isGetter && meth.accessed != NoSymbol && meth.accessed.asTerm.isVar => meth }
      if (varGetters.nonEmpty) {
        c.abort(c.enclosingPosition, "not immutable")
      }
    }
  }

  implicit def generate[T]: Immutable[T] = macro genQuery[T, this.type]

  implicit val intIsImmutable: Immutable[Int] = new Immutable[Int] { def noop(x: Int) = {} }

  implicit val stringHasShow: Immutable[String] = new Immutable[String] { def noop(x: String) = {} }
}


trait Scale[T] {
  def scale(x: T): T
}

/* This is a transformation. It multiplies all integers by 10. All other objects
 * are copied unchanged.
 */
object Scale extends Transform {
  def mkTrees[C <: Context with Singleton](c: C): Trees[C] =
    new Trees(c)

  class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) {
    def invoke(inst: c.Tree, value: c.Tree): c.Tree = {
      import c.universe._
      q"$inst.scale($value)"
    }
  }

  implicit def generate[T]: Scale[T] = macro genTransform[T, this.type]

  implicit val intHasScale: Scale[Int] = new Scale[Int] {
    def scale(x: Int): Int = x * 10
  }

  implicit val stringHasScale: Scale[String] = new Scale[String] {
    def scale(x: String): String = x
  }
}
