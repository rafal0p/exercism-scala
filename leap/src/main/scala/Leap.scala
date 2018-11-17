object Leap {

  def leapYear(year: Int): Boolean =
    divisibleBy(4, year) &&
      (!divisibleBy(100, year) || divisibleBy(400, year))

  private def divisibleBy(divisor: Int, dividend: Int): Boolean = dividend % divisor == 0
}