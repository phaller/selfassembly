import org.junit.Test
import selfassembly.examples._

case class Person2(name: String, age: Int)

class ShowSpec {
  @Test def testInt() {
    val ts: Show[Int] = implicitly[Show[Int]]
    val s = ts.show(42)
    assert(s == "42")
  }

  @Test def testCaseClass() {
    val p = Person2("joe", 40)
    val ts2: Show[Person2] = implicitly[Show[Person2]]
    val s2 = ts2.show(p)
    assert(s2 == "Person2(joe, 40)")
  }
}
