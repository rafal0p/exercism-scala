abstract case class Clock(hour: Int, minute: Int) {
  require(hour >= 0)
  require(hour <= 24)
  require(minute >= 0)
  require(minute <= 60)

  def +(that: Clock): Clock = Clock(hour + that.hour, minute + that.minute)

  def -(that: Clock): Clock = Clock(hour - that.hour, minute - that.minute)
}

object Clock {
  def apply(minute: Int): Clock = apply(0, minute)

  def apply(hour: Int, minute: Int): Clock = {
    val sumOfMinutes = hour * 60 + minute
    val positiveSumOfMinutes = turnForwardIfNegative(sumOfMinutes)
    val sumOfMinutesWithoutFullDays = positiveSumOfMinutes % minutesInDay
    val totalHours = sumOfMinutesWithoutFullDays / 60
    val totalMinutes = sumOfMinutesWithoutFullDays % 60
    new Clock(totalHours, totalMinutes) {}
  }

  private def turnForwardIfNegative(sumOfMinutes: Int) = {
    if (sumOfMinutes > 0)
      sumOfMinutes
    else
      sumOfMinutes + minutesInDay * howManyDays(sumOfMinutes)
  }

  private def howManyDays(sumOfMinutes: Int) = Math.abs(sumOfMinutes) % minutesInDay

  private val minutesInDay = 60 * 24
}
