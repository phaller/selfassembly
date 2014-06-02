/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 *
 * @author Philipp Haller
 * @author Heather Miller
 */
package selfassembly

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

/** Methods in this singleton object should ideally be part of the
 *  reflection API.
 */
object core {

  def constant[T: c.WeakTypeTag](c: Context)(value: T): c.Expr[T] =
    c.Expr[T](c.universe.Literal(c.universe.Constant(value)))

  def implicitFastTypeTag(c: Context)(tpe: c.Type): c.Expr[examples.pickling.FastTypeTag[_]] = {
    import c.universe._
    c.Expr[examples.pickling.FastTypeTag[_]](
      q"implicitly[selfassembly.examples.pickling.FastTypeTag[$tpe]]"
    )
  }
}
