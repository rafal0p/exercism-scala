object Hamming {
  def distance(left: String, right: String): Option[Int] =
    Some(() =>
      (left zip right)
        .map {
          case (l, r) if l == r => 0
          case _ => 1
        }
        .sum
    ).filter(_ => left.length == right.length)()

}