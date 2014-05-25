import genlib.examples._

case class Person4(name: String, age: Int, pocket: Pocket)

case class Pocket(/*var */contents: Int)

object Test4 extends App {

  val p = Person4("joe", 40, Pocket(10))
  val ts: Immutable[Int] = implicitly[Immutable[Int]]

  val ts2: Immutable[Person4] = implicitly[Immutable[Person4]]
}
