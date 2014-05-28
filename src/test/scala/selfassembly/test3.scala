import org.junit.Test
import selfassembly.examples._

case class Person3(var name: String, var age: Int)

class ScaleSpec {
  @Test def testInt() {
    val ts: Scale[Int] = implicitly[Scale[Int]]
    val s = ts.scale(42)
    assert(s == 420)
  }

  @Test def testCaseClass() {
    val p = Person3("joe", 40)
    val ts2: Scale[Person3] = implicitly[Scale[Person3]]
    val s2 = ts2.scale(p)
    assert(s2.toString == "Person3(joe,400)")
  }
}
