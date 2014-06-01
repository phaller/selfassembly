package test.pickling

import org.junit.Test
import selfassembly.examples.pickling._
import json._
import selfassembly.SimpleRegistry

final case class Person7(name: String, age: Int)

case class C2(arr: Array[Int]) { override def toString = s"""C(${arr.mkString("[", ",", "]")})""" }

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

  @Test def testArrayInt() {
    val a = Array[Int](1, 2, 3)
    val inst = implicitly[SPickler[Array[Int]]]
    val builder: JSONPickleBuilder = pickleFormat.createBuilder()
    builder.hintTag(implicitly[FastTypeTag[Array[Int]]])
    inst.pickle((a -> builder))
    val s = builder.result.value
    val pickle = JSONPickle(s)
    val up = pickle.unpickle[Array[Int]]
    assert(up.mkString("[", ",", "]") == a.mkString("[", ",", "]"))
  }

  @Test def testCaseClassArrayInt() {
    val a = C2(Array[Int](1, 2, 3))
    val inst = implicitly[SPickler[C2]]
    val builder: JSONPickleBuilder = pickleFormat.createBuilder()
    builder.hintTag(implicitly[FastTypeTag[C2]])
    inst.pickle((a -> builder))
    val s = builder.result.value
    val pickle = JSONPickle(s)
    val up = pickle.unpickle[C2]
    assert(up.toString == a.toString)
  }
}
