object PhoneNumber {
  def clean(raw: String): Option[String] = {
    val number = trimCountryCode(raw.filter(_.isDigit))
    if (areaCode(number).startsWithAnyOf(Set('0', '1'))
      || localNumber(number).startsWithAnyOf(Set('0', '1')))
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

  private implicit class OptionExtension(s: Option[String]) {
    def startsWithAnyOf(chars: Set[Char]): Boolean =
      s.map(_ (0)).exists(chars.contains)
  }

}
