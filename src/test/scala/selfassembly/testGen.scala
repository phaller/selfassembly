import org.junit.Test
import selfassembly.SimpleRegistry
import selfassembly.examples._


case class Contains(var e: Employee, var other: Int)


class GenSpec {
  @Test def testGenCaseClass() {
    val e = Employee("joe", 40)

    val genP: Gen[Employee] = implicitly[Gen[Employee]]

    val strings = (1 to 5).map { i =>
      val e2 = genP.sample(e)
      println(e2)
      e2.toString
    }

    assert(strings.forall(s => s.startsWith("Employee")))
  }

  @Test def testGenComplexerCaseClass() {
    val e = Contains(Employee("joe", 40), 10)

    val gen: Gen[Contains] = implicitly[Gen[Contains]]

    val strings = (1 to 5).map { i =>
      val e2 = gen.sample(e)
      println(e2)
      e2.toString
    }

    assert(strings.forall(s => s.startsWith("Contains(Employee(")))
  }
}
