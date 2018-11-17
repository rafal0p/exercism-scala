abstract case class Clock(hour: Int, minute: Int) {
  require(hour >= 0)
  require(hour <= 24)
  require(minute >= 0)
  require(minute <= 60)

  def +(that: Clock) = Nil

  def -(that: Clock) = Nil
}

object Clock {
  def apply(hour: Int, minute: Int): Clock =
    new Clock(
      hour % 24 + minute / 60,
      minute % 60
    ) {}

  def apply(minute: Int): Clock = new Clock(0, minute) {}
}
