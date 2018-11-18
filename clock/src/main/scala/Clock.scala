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
    val minutesSum = hour * 60 + minute
    val positiveMinutesSum = turnForwardIfNegative(minutesSum)
    val minutesSumWithoutFullDays = positiveMinutesSum % minutesInDay
    val totalHours = minutesSumWithoutFullDays / 60
    val totalMinutes = minutesSumWithoutFullDays % 60
    new Clock(totalHours, totalMinutes) {}
  }

  private def turnForwardIfNegative(minutes: Int) = {
    if (minutes > 0)
      minutes
    else
      minutes + minutesInDay * (1 + countDaysIn(minutes))
  }

  private def countDaysIn(minutes: Int) = Math.abs(minutes) / minutesInDay

  private val minutesInDay = 60 * 24
}
