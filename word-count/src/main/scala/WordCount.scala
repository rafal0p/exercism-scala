case class WordCount(sentence: String) {
  private val whitespaces = Array(' ', ',', '\n')

  private val trimStartAndEnd: String => String =
    (word: String) => {
      val firstLetterIdx = word.indexWhere(_.isLetterOrDigit)
      val lastLetterIdx = word.lastIndexWhere(_.isLetterOrDigit)
      word.slice(firstLetterIdx, lastLetterIdx + 1)
    }

  val countWords: Map[String, Int] =
    sentence
      .toLowerCase
      .split(whitespaces)
      .filterNot(_.isEmpty)
      .map(trimStartAndEnd)
      .groupBy(identity)
      .mapValues(_.length)
}
