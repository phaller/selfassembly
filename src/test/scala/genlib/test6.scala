import selfassembly.examples._

sealed trait Person6
case class Employee(name: String, age: Int) extends Person6

object Test6 extends App {

  val e = Employee("joe", 40)

  val ts: Show[Person6] = implicitly[Show[Person6]]
  val s = ts.doIt(e)
  println(s)
}
