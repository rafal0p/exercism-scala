object Bob {

  def response(statement: String): String = {
    if (isShout(statement) && isQuestion(statement))
      onShoutedQuestion
    else if (isShout(statement))
      onShout
    else if (isQuestion(statement))
      onQuestion
    else if (isSilence(statement))
      onSilence
    else
      onAnythingElse
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