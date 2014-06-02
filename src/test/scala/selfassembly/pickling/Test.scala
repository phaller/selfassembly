package test.pickling

import org.junit.Test
import selfassembly.examples.pickling._
import json._
import selfassembly.SimpleRegistry

final case class Person7(name: String, age: Int)

case class C2(arr: Array[Int]) { override def toString = s"""C(${arr.mkString("[", ",", "]")})""" }

final case class Position(person: Person7, title: String)

sealed abstract class Person9 {
  def name: String
  def age: Int
}

final case class Firefighter(name: String, age: Int, since: Int) extends Person9

final case class Position2(person: Person9, title: String)

class PicklingSpec {
  implicit val reg = new SimpleRegistry[SPickler]

  @Test def testCaseClass() {
    val p = Person7("Jim", 40)

    val inst = implicitly[SPickler[Person7]]

    val builder: JSONPickleBuilder = pickleFormat.createBuilder()
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
    inst.pickle((a -> builder))
    val s = builder.result.value
    val pickle = JSONPickle(s)
    val up = pickle.unpickle[C2]
    assert(up.toString == a.toString)
  }

  @Test def testPickleMethod() {
    val p = Person7("Jim", 40)
    val pickled = p.pickle
    val s = pickled.value
    assert(s == """{
  "tpe": "test.pickling.Person7",
  "name": "Jim",
  "age": 40
}""")
    val pickle = JSONPickle(s)
    val up = pickle.unpickle[Person7]
    assert(p == up)
  }

  @Test def testFinalCaseClassField() {
    val p = Person7("Jim", 40)
    val pos = Position(p, "CEO")
    val inst = implicitly[SPickler[Position]]
    val builder: JSONPickleBuilder = pickleFormat.createBuilder()
    inst.pickle((pos -> builder))
    val s = builder.result.value
    assert(s == """{
  "tpe": "test.pickling.Position",
  "person": {
    "name": "Jim",
    "age": 40
  },
  "title": "CEO"
}""")
    val pickle = JSONPickle(s)
    val up = pickle.unpickle[Position]
    assert(pos == up)
  }

  @Test def testAbstractClassField() {
    val p = Firefighter("Jim", 40, 2014)
    val pos = Position2(p, "CEO")
    val inst = implicitly[SPickler[Position2]]
    val builder: JSONPickleBuilder = pickleFormat.createBuilder()
    inst.pickle((pos -> builder))
    val s = builder.result.value
    assert(s == """{
  "tpe": "test.pickling.Position2",
  "person": {
    "tpe": "test.pickling.Firefighter",
    "name": "Jim",
    "age": 40,
    "since": 2014
  },
  "title": "CEO"
}""")
  }
}
