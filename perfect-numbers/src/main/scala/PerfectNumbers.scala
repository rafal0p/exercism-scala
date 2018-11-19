object PerfectNumbers {

  def classify(n: Int): Either[String, NumberType.Value] = {
    if (n <= 0)
      Left("Classification is only possible for natural numbers.")
    else
      factors(n).sum match {
        case sum if sum == n => Right(NumberType.Perfect)
        case sum if sum > n => Right(NumberType.Abundant)
        case sum if sum < n => Right(NumberType.Deficient)
      }
  }

  private def factors(n: Int) = (1 to n / 2).filter(n % _ == 0)
}

object NumberType extends Enumeration {
  type NumberType = Value
  val Perfect, Abundant, Deficient = Value
}
