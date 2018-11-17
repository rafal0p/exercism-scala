object CollatzConjecture {
  def steps(i: Int): Option[Int] = {
    if (i <= 0)
      None
    else {
      var total = 0
      var current = i
      while (current != 1) {
        current = collatz(current)
        total += 1
      }
      Some(total)
    }
  }

  private def collatz(n: Int) =
    n match {
      case _ if n % 2 == 0 => n / 2
      case _ => 3 * n + 1
    }
}