object Bob {

  def response(statement: String): String =
    statement match {
      case isShoutedQuestion(res) => res
      case isShout(res) => res
      case isSilence(res) => res
      case isQuestion(res) => res
      case _ => onAnythingElse
    }

  object isSilence {
    def unapply(str: String): Option[String] = ifIs(silence(str), onSilence)
  }

  object isQuestion {
    def unapply(str: String): Option[String] = ifIs(question(str), onQuestion)
  }

  object isShout {
    def unapply(str: String): Option[String] = ifIs(shout(str), onShout)
  }

  object isShoutedQuestion {
    def unapply(str: String): Option[String] = ifIs(shout(str) && question(str), onShoutedQuestion)
  }

  private def shout(statement: String) =
    statement.exists(_.isLetter) && statement.equals(statement.toUpperCase())

  private def question(statement: String) =
    statement.trim().endsWith("?")

  private def silence(str: String) = str.trim() == ""

  private def ifIs(condition: Boolean, some: String): Option[String] = {
    if (condition)
      Some(some)
    else
      None
  }

  private val onShout = "Whoa, chill out!"
  private val onQuestion = "Sure."
  private val onShoutedQuestion = "Calm down, I know what I'm doing!"
  private val onSilence = "Fine. Be that way!"
  private val onAnythingElse = "Whatever."
}