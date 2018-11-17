abstract case class Clock(hour: Int, minute: Int) {

  def +(that: Clock) = Nil

  def -(that: Clock) = Nil
}

object Clock {
  def apply(hour: Int, minute: Int): Clock = new Clock(hour % 24, minute) {}

  def apply(minute: Int): Clock = new Clock(0, minute) {}
}
