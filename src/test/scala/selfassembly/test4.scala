import org.junit.Test
import selfassembly.examples._

case class Person4(name: String, age: Int, pocket: Pocket)

case class Pocket(/*var */contents: Int)

class ImmutableSpec {
  @Test def test() {
    val p = Person4("joe", 40, Pocket(10))
    val ts: Immutable[Int] = implicitly[Immutable[Int]]

    val ts2: Immutable[Person4] = implicitly[Immutable[Person4]]
    assert(true)
  }
}
