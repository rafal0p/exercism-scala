

object SumOfMultiples {

  def sum(factors: Set[Int], limit: Int): Int = {
    factors
      .flatMap(multiplesUpTo(limit))
      .sum
  }

  private def multiplesUpTo(limit: Int) = {
    n: Int =>
      Stream.from(1)
        .map(i => n * i)
        .takeWhile(_ < limit)
  }
}

