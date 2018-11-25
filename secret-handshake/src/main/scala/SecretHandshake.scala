import scala.annotation.tailrec

object SecretHandshake {
  def commands(code: Int): List[String] = {
    @tailrec
    def _commands(code: Int, acc: List[String], shouldReverse: Boolean): List[String] = code match {
      case 0 => if (shouldReverse) acc.reverse else acc
      case _ if code / 16 >= 1 => _commands(code - 16, acc, true)
      case _ if code / 8 >= 1 => _commands(code - 8, "jump" :: acc, shouldReverse)
      case _ if code / 4 >= 1 => _commands(code - 4, "close your eyes" :: acc, shouldReverse)
      case _ if code / 2 >= 1 => _commands(code - 2, "double blink" :: acc, shouldReverse)
      case 1 => _commands(code - 1, "wink" :: acc, shouldReverse)
    }

    _commands(code, List(), shouldReverse = false)
  }
}
