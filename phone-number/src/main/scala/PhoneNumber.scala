object PhoneNumber {
  def clean(raw: String): Option[String] = {
    val number = trimCountryCode(raw.filter(_.isDigit))
    if (startsWithAnyOf(Set('0', '1'), areaCode(number))
      || startsWithAnyOf(Set('0', '1'), localNumber(number)))
      None
    else
      number
  }

  private def trimCountryCode(digits: String) = {
    if (digits.length == 11 && digits(0) == '1')
      Some(digits.drop(1))
    else if (digits.length == 10)
      Some(digits)
    else
      None
  }

  private def localNumber(number: Option[String]) = number.map(_.drop(3))

  private def areaCode(number: Option[String]) = number.map(_.take(3))

  private def startsWithAnyOf(chars: Set[Char], string: Option[String]) =
    string.map(_ (0)).exists(chars.contains)
}
