import scala.util.Random

class Robot {
  private var _name = letters + numbers

  def name: String = _name

  private def numbers = {
    Random.nextInt(1000).formatted("%03d")
  }

  private def letters = {
    Random.alphanumeric.dropWhile(_.isDigit).map(_.toUpper).take(2).mkString
  }

  def reset(): Unit = {
    _name = letters + numbers
  }
}