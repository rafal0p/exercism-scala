object Bob {

  def response(statement: String): String =
    statement match {
      case _ if isShout(statement) && isQuestion(statement) => onShoutedQuestion
      case _ if isShout(statement) => onShout
      case _ if isQuestion(statement) => onQuestion
      case _ if isSilence(statement) => onSilence
      case _ => onAnythingElse
    }

  private def isShout(statement: String) =
    statement.exists(_.isLetter) && statement.equals(statement.toUpperCase())

  private def isQuestion(statement: String) =
    statement.trim().endsWith("?")

  private def isSilence(statement: String) =
    statement.trim() == ""

  private val onShout = "Whoa, chill out!"
  private val onQuestion = "Sure."
  private val onShoutedQuestion = "Calm down, I know what I'm doing!"
  private val onSilence = "Fine. Be that way!"
  private val onAnythingElse = "Whatever."
}