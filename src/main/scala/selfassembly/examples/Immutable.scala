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
  def mkTrees[C <: Context with Singleton](c: C) =
    new Trees(c)

  class Trees[C <: Context with Singleton]
    (override val c: C) extends super.Trees(c) {
    def check(tpe: c.Type): Unit = {
      import c.universe._

      if (tpe.typeSymbol.isClass &&
          !tpe.typeSymbol.asClass.isFinal &&
          !tpe.typeSymbol.asClass.isCaseClass) {
        c.abort(c.enclosingPosition, "instances of non-final or non-case class not guaranteed to be immutable")
      } else {
        // if tpe has var, abort
        val allAccessors =
          tpe.decls collect { case sym: MethodSymbol if sym.isAccessor || sym.isParamAccessor => sym }
        val varGetters =
          allAccessors collect { case sym if sym.isGetter && sym.accessed != NoSymbol && sym.accessed.asTerm.isVar => sym }
        if (varGetters.nonEmpty)
          c.abort(c.enclosingPosition, "not immutable")
      }
    }
  }

  implicit def generate[T]: Immutable[T] = macro genQuery[T, this.type]

  implicit val intIsImmutable: Immutable[Int] = new Immutable[Int] { def noop(x: Int) = {} }

  implicit val stringIsImmutable: Immutable[String] = new Immutable[String] { def noop(x: String) = {} }
}
