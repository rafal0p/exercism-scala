import scala.annotation.tailrec

object CollatzConjecture {
  def steps(i: Int): Option[Int] = {
    val (total, _) = countSteps(0, i)
    total
  }

  @tailrec
  private def countSteps(total: Int, current: Int): (Option[Int], Int) =
    current match {
      case i if i <= 0 => (None, i)
      case 1 => (Some(total), 1)
      case _ => countSteps(total + 1, collatz(current))
    }

  private def collatz(n: Int) =
    n match {
      case _ if n % 2 == 0 => n / 2
      case _ => 3 * n + 1
    }
}