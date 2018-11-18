object Etl {
  def transform(scores: Map[Int, Seq[String]]): Map[String, Int] =
    scores.flatMap {
      case (score, letters) =>
        letters
          .map(_.toLowerCase)
          .map(letter => (letter, score))
    }
}
