package person

abstract class Person {
  def name: String
  def age: Int
}

case class Employee(n: String, a: Int, s: Int)
  extends Person {
  def name = n
  def age = a
}
