import selfassembly.examples._

case class Person5(name: String, age: Int, var parent: Person5)

object Test5 extends App {

  val ts: Show[Int] = implicitly[Show[Int]]
  val s = ts.doIt(42)
  println(s)

  // joe has a very interesting relationship with himself
  val p = Person5("joe", 40, null)
  p.parent = p

  val ts2: Show[Person5] = implicitly[Show[Person5]]
  val s2 = ts2.doIt(p)
  println(s2)
}
