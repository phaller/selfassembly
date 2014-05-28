import org.junit.Test
import selfassembly.examples._

case class Person(name: String, age: Int)

class ToStringSpec {
  @Test def testInt() {
    val ts: ToString[Int] = implicitly[ToString[Int]]
    val s = ts.mkString(42)
    assert(s == "42")
  }

  @Test def testCaseClass() {
    val p = Person("joe", 40)
    val ts2: ToString[Person] = implicitly[ToString[Person]]
    val s2 = ts2.mkString(p)
    assert(s2 == "Person(joe, 40)")
  }
}
