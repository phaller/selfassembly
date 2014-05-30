/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 */
package selfassembly.examples.pickling

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import selfassembly.{Queryable, Query}

trait SPickler[T] extends Queryable[(T, PBuilder), Unit] {
  //TODO:
  //val format: PickleFormat
  def pickle(visitee: (T, PBuilder)): Unit
}

object SPickler extends Query[Unit] {
  import internal._ // for isEffectivelyFinal

  def mkTrees[C <: Context with Singleton](c: C): Trees[C] =
    new Trees(c)

  class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c) {
    import c.universe._

    // need an operation that tells us how to go from a field value
    // to the value that we have to pass to the type class
    // fld -->  (fld, builder)
    override def inject(fieldValue: c.Tree): c.Tree =
      q"($fieldValue -> visitee._2)"

    override def project(param: c.Tree): c.Tree =
      q"$param._1"

    // for now, innerMostPickleLogic must use `b` as the name of the builder! (TODO)
    override def putField(getterLogic: Tree, innerMostPickleLogic: c.Tree, nameOfSubPicklee: c.TermName,
                          fieldNameString: String, fieldTpe: c.Type): c.Tree = {
      val builder = q"visitee._2"
      def wrap(pickleLogic: Tree) = q"""
        $builder.putField($fieldNameString, b => $pickleLogic)
      """
      wrap {
        if (fieldTpe.typeSymbol.isEffectivelyFinal) q"""
          b.hintStaticallyElidedType()
          val $nameOfSubPicklee: $fieldTpe = $getterLogic
          $innerMostPickleLogic
        """ else q"""
          val $nameOfSubPicklee: $fieldTpe = $getterLogic
          if ($nameOfSubPicklee == null || $nameOfSubPicklee.getClass == classOf[$fieldTpe]) b.hintDynamicallyElidedType() else ()
          $nameOfSubPicklee.pickleInto(b)
        """
      }
    }

    override def preInvoke(fieldTpe: c.Type): c.Tree = {
      val secondTree = if (fieldTpe.typeSymbol.isEffectivelyFinal)
        q"visitee._2.hintStaticallyElidedType()" else q""
      q"""
        visitee._2.hintTag(implicitly[selfassembly.examples.pickling.FastTypeTag[$fieldTpe]])
        $secondTree
      """
    }

    def combine(left: c.Expr[Unit], right: c.Expr[Unit]): c.Expr[Unit] =
      c.Expr(q"$left ; $right")

    def delimit(tpe: c.Type): (c.Expr[Unit], c.Expr[Unit], c.Expr[Unit]) = {
      val typeString = tpe.typeSymbol.name.toString
      val first = c.Expr(q"""
        visitee._2.beginEntry(visitee._1)
      """)
      (first, c.Expr(q"; {}"), c.Expr(q"visitee._2.endEntry()"))
    }
  }

  implicit def generate[T]: SPickler[T] = macro genQuery[T, this.type]

  class PrimitivePickler[T] extends SPickler[T] {
    val format = null // not used
    def pickle(pb: (T, PBuilder)): Unit = {
      pb._2.beginEntry(pb._1)
      pb._2.endEntry()
    }
    def apply(visitee: (T, PBuilder), visited: Set[Any]): Unit =
      pickle(visitee)
  }

  implicit val intPickler: SPickler[Int] = new PrimitivePickler[Int]
  implicit val nullPicklerUnpickler: SPickler[Null] = new PrimitivePickler[Null]
  implicit val stringPickler: SPickler[String] =  new PrimitivePickler[String]
}
