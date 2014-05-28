package person

import org.junit.Test
import selfassembly.SimpleRegistry
import selfassembly.examples._


// File PersonA.scala:
abstract class Person {
  def name: String
  def age: Int
}

case class Employee(n: String, a: Int, s: Int)
  extends Person {
  def name = n
  def age = a
}

// File PersonB.scala:
class Volunteer(n: String, a: Int, s: Int) extends Person {
  def name = n
  def age = a
  def since = s
}

case class Firefighter(n: String, a: Int, s: Int)
  extends Volunteer(n, a, s)


class DispatchSpec {
  @Test def testDispatch() {
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
}
