import org.junit.Test
import selfassembly.examples._

case class Person5(name: String, age: Int, var parent: Person5)

class CycleSpec {
  @Test def testInt() {
    val ts: Show[Int] = implicitly[Show[Int]]
    val s = ts.show(42)
    assert(s == "42")
  }

  @Test def testCaseClass() {
    // joe has a very interesting relationship with himself
    val p = Person5("joe", 40, null)
    p.parent = p

    val ts2: Show[Person5] = implicitly[Show[Person5]]
    val s2 = ts2.show(p)
    assert(s2 == "Person5(joe, 40, Person5())")
  }
}
