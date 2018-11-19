object PhoneNumber {
  def clean(dirty: String): Option[String] = {
    val digits = dirty.filter(_.isDigit)
    digits.toList match {
      case '1' :: t if t.length == 10 => Some(t.mkString)
      case '0' :: _ => None
      case '1' :: _ => None
      case _ :: _ :: _ :: '0' :: _ => None
      case _ :: _ :: _ :: '1' :: _ => None
      case _ :: t if t.length == 9 => Some(digits)
      case _ => None
    }
  }
}
