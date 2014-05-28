import selfassembly._
import selfassembly.examples._

sealed trait Person6
case class Employee(name: String, age: Int) extends Person6

object Test6 extends App {
  implicit val reg = new SimpleRegistry[Show]

  val e = Employee("joe", 40)

  val ts: Show[Person6] = implicitly[Show[Person6]]
  val s = ts.show(e)
  println(s)
  assert(s == "Employee(joe, 40)")
}
