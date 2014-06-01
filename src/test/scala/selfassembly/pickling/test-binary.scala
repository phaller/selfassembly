package test.pickling

import org.junit.Test
import selfassembly.examples.pickling._
import selfassembly.SimpleRegistry

case class Person2(name: String, age: Int)

class PicklingBinarySpec {
  implicit val reg = new SimpleRegistry[SPickler]

  @Test def testCaseClass() {
    val p = Person2("Jim", 40)

    implicit val format = binary.pickleFormat
    val inst = implicitly[SPickler[Person2]]

    val builder: binary.BinaryPickleBuilder = format.createBuilder()
    inst.pickle((p -> builder))
    val a: Array[Byte] = builder.result.value
    val s = a.mkString("[", ",", "]")
    // result self-assembly:  [0,0,0,21,116,101,115,116,46,112,105,99,107,108,105,110,103,46,80,101,114,115,111,110,50,0,0,0,3,74,105,109,0,0,0,40]
    // result scala-pickling: [0,0,0,21,116,101,115,116,46,112,105,99,107,108,105,110,103,46,80,101,114,115,111,110,50,0,0,0,3,74,105,109,0,0,0,40]
    assert(s == "[0,0,0,21,116,101,115,116,46,112,105,99,107,108,105,110,103,46,80,101,114,115,111,110,50,0,0,0,3,74,105,109,0,0,0,40]")
  }
}
