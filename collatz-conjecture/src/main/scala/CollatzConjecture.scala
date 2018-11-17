object CollatzConjecture {

  def steps(i: Int): Option[Int] = {
    if (i <= 0)
      None
    else
      Some(Stream.iterate(i)(collatz).takeWhile(_ != 1).length)
  }

  private def collatz(n: Int) =
    n match {
      case _ if n % 2 == 0 => n / 2
      case _ => 3 * n + 1
    }
}