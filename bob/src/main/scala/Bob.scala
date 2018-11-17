object Bob {

  def response(statement: String): String =
    statement match {
      case IsShoutedQuestion() => onShoutedQuestion
      case IsShout() => onShout
      case IsSilence() => onSilence
      case IsQuestion() => onQuestion
      case _ => onAnythingElse
    }

  object IsShoutedQuestion {
    def unapply(str: String): Boolean =
      IsShout.unapply(str) && IsQuestion.unapply(str)
  }

  object IsShout {
    def unapply(str: String): Boolean = str.exists(_.isLetter) && str.equals(str.toUpperCase())
  }

  object IsQuestion {
    def unapply(str: String): Boolean = str.trim().endsWith("?")
  }

  object IsSilence {
    def unapply(str: String): Boolean = str.trim() == ""
  }

  private val onShout = "Whoa, chill out!"
  private val onQuestion = "Sure."
  private val onShoutedQuestion = "Calm down, I know what I'm doing!"
  private val onSilence = "Fine. Be that way!"
  private val onAnythingElse = "Whatever."
}