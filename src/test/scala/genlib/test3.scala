import selfassembly.examples._

case class Person3(var name: String, var age: Int)

object Test3 extends App {

  val p = Person3("joe", 40)
  val ts: Scale[Int] = implicitly[Scale[Int]]
  val s = ts.scale(42)
  println(s)

  val ts2: Scale[Person3] = implicitly[Scale[Person3]]
  val s2 = ts2.scale(p)
  println(s2)
}
