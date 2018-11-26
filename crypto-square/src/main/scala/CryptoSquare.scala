object CryptoSquare {
  def ciphertext(rawText: String): String = {
    val text = rawText.toLowerCase.filter(_.isLetterOrDigit)
    val colCount = columns(text.length)
    val groups = text.grouped(colCount).toSeq
    val rowCount = rows(groups)
    (0 until rowCount)
      .map(r =>
        groups
          .map(s => s.applyOrElse(r, (_: Int) => ' '))
          .mkString
      )
      .mkString(" ")
  }

  private def rows(groups: Seq[String]) =
    (groups.map(_.length) :+ 1).max

  private def columns(n: Int): Int =
    Stream
      .from(1)
      .dropWhile(i => i * i < n)
      .head
}
