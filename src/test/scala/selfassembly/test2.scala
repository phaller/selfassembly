import selfassembly.examples._

case class Person2(name: String, age: Int)

object Test2 extends App {

  val p = Person2("joe", 40)
  val ts: Show[Int] = implicitly[Show[Int]]
  val s = ts.doIt(42)
  println(s)

  val ts2: Show[Person2] = implicitly[Show[Person2]]
  val s2 = ts2.doIt(p)
  println(s2)
  assert(s2 == "Person2(joe, 40)")
}
