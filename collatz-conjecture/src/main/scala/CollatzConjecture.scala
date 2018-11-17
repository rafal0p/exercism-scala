import scala.annotation.tailrec

object CollatzConjecture {
  @tailrec
  def steps(n: Int, count: Int = 0): Option[Int] = n match {
    case 1 => Some(count)
    case x if x <= 0 => None
    case x if x % 2 == 0 => steps(x / 2, count + 1)
    case x => steps(3 * x + 1, count + 1)
  }
}