object CollatzConjecture {

  def steps(i: Int): Option[Int] = {
    Option(i)
      .filter(_ > 0)
      .map(x => Stream.iterate(x)(collatz).takeWhile(_ != 1).length)
  }

  private def collatz(n: Int) =
    n match {
      case _ if n % 2 == 0 => n / 2
      case _ => 3 * n + 1
    }
}