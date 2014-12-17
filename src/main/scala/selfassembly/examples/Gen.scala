package selfassembly
package examples

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import scala.util.Random


trait Gen[T] extends Queryable[T, T] {
  def sample(x: T): T
}

object Gen extends Transform {
  def mkTrees[C <: Context with Singleton](c: C): Trees[C] =
    new Trees(c)

  class Trees[C <: Context with Singleton](override val c: C) extends super.Trees(c)

  implicit def generate[T]: Gen[T] = macro genTransform[T, this.type]

  implicit val intHasGen: Gen[Int] = new Gen[Int] {
    def sample(x: Int): Int = Random.nextInt()
    def apply(visitee: Int, visited: Set[Any]): Int = sample(visitee)
  }

  implicit val stringHasGen: Gen[String] = new Gen[String] {
    def sample(x: String): String = {
      val len = Random.nextInt(10)
      val chars = ((1 to 10).map { i =>
        val ch = 65 + Random.nextInt(26)
        ch.asInstanceOf[Char]
      }).toArray
      new String(chars)
    }
    def apply(visitee: String, visited: Set[Any]): String = sample(visitee)
  }

}
