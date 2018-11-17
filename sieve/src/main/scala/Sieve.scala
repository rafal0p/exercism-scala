object Sieve {

  def primes(max: Int): List[Int] = {
    findPrimes(List(), (2 to max).toList)
  }

  private def findPrimes(foundSoFar: List[Int],
                         list: List[Int]): List[Int] = list match {
    case head :: tail =>
      findPrimes(foundSoFar :+ head, tail.filter(multiples(of = head)))
    case _ => foundSoFar
  }

  private def multiples(of: Int) = {
    i: Int => i % of != 0
  }
}