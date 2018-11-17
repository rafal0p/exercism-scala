abstract case class Clock(hour: Int, minute: Int) {
  require(hour >= 0)
  require(hour <= 24)
  require(minute >= 0)
  require(minute <= 60)

  def +(that: Clock) = Nil

  def -(that: Clock) = Nil
}

object Clock {
  def apply(hour: Int, minute: Int): Clock = {
    val sumOfMinutes = hour * 60 + minute
    val sumOfMinutesWithoutFullDays = sumOfMinutes % (24 * 60)
    val totalHours = sumOfMinutesWithoutFullDays / 60
    val totalMinutes = sumOfMinutesWithoutFullDays % 60
    new Clock(totalHours, totalMinutes) {}
  }

  def apply(minute: Int): Clock = new Clock(0, minute) {}
}
