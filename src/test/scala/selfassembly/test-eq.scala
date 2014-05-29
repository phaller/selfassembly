import org.junit.Test
import selfassembly.examples._

case class Person8(name: String, age: Int)

class EqSpec {
  @Test def testInt() {
    val x = 42
    val ts: Eq[Int] = implicitly[Eq[Int]]
    val s = ts.eq((x, 42))
    assert(s == true)
  }

  @Test def testCaseClass() {
    val p = Person8("joe", 40)
    val n = "joe"
    val a = 40
    val p2 = Person8(n, a)

    val ts2: Eq[Person8] = implicitly[Eq[Person8]]
    val s2 = ts2.eq((p, p2))

    assert(s2 == true)

    val p3 = Person8(n, a + 1)
    val s3 = ts2.eq((p, p3))

    assert(s3 == false)
  }

  testInt()
  testCaseClass()
}
