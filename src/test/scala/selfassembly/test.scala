import selfassembly.examples._

case class Person(name: String, age: Int)

object Test extends App {

  val p = Person("joe", 40)
  val ts: ToString[Int] = implicitly[ToString[Int]]
  val s = ts.mkString(42)
  println(s)

  val ts2: ToString[Person] = implicitly[ToString[Person]]
  val s2 = ts2.mkString(p)
  println(s2)
  assert(s2 == "Person(joe, 40)")
}
