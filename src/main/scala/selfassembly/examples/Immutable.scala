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


trait Immutable[T] {
  def noop(x: T): Unit
}

object Immutable extends Property[Unit] {
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
