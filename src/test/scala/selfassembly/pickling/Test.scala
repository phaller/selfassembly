package test.pickling

import org.junit.Test
import selfassembly.examples.pickling._
import json._
import selfassembly.SimpleRegistry

final case class Person7(name: String, age: Int)

class PicklingSpec {
  implicit val reg = new SimpleRegistry[SPickler]

  @Test def testCaseClass() {
    val p = Person7("Jim", 40)

    val inst = implicitly[SPickler[Person7]]

    val builder: JSONPickleBuilder = pickleFormat.createBuilder()
    builder.hintTag(implicitly[FastTypeTag[Person7]])
    inst.pickle((p -> builder))
    val s = builder.result.value
    assert(s == """{
  "tpe": "test.pickling.Person7",
  "name": "Jim",
  "age": 40
}""")

    val pickle = JSONPickle(s)

    val up = pickle.unpickle[Person7]

    assert(p == up)
  }
}
