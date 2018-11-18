object Hamming {
  def distance(left: String, right: String): Option[Int] =
    Option(left.length == right.length)
      .filter(identity)
      .map(_ =>
        (left zip right)
          .map {
            case (l, r) if l == r => 0
            case _ => 1
          }
          .sum
      )
}