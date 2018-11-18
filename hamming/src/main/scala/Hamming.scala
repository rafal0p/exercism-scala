object Hamming {
  def distance(left: String, right: String): Option[Int] =
    if (left.length != right.length)
      None
    else
      Some(
        left.zip(right)
          .map {
            case (l, r) if l == r => 0
            case _ => 1
          }
          .sum)
}