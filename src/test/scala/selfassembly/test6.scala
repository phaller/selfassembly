import org.junit.Test
import selfassembly.SimpleRegistry
import selfassembly.examples._

sealed trait Person6
case class Employee(name: String, age: Int) extends Person6

class SubtypingSpec {
  @Test def testSealedTraitCaseClass() {
    implicit val reg = new SimpleRegistry[Show]

    val e = Employee("joe", 40)

    val ts: Show[Person6] = implicitly[Show[Person6]]
    val s = ts.show(e)
    assert(s == "Employee(joe, 40)")
  }
}
