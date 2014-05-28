package person

class Volunteer(n: String, a: Int, s: Int) extends Person {
  def name = n
  def age = a
  def since = s
}

case class Firefighter(n: String, a: Int, s: Int)
  extends Volunteer(n, a, s)
