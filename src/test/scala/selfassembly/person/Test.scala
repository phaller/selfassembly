package person

import selfassembly._
import selfassembly.examples._

object Test extends App {
  implicit val reg = new SimpleRegistry[Show]

  val em = Employee("Dave", 35, 80000)
  val ff = Firefighter("Jim", 40, 2004)
  val inst = implicitly[Show[Person]]
  println(inst.show(em))
  // prints: Employee(Dave, 35, 80000)
  println(inst.show(ff))
  // prints: Firefighter(Jim, 40, 2004)

  assert(inst.show(em) == "Employee(Dave, 35, 80000)")
  assert(inst.show(ff) == "Firefighter(Jim, 40, 2004)")
}
