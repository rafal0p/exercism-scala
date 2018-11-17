object Sieve {
  def primes(max: Int): List[Int] = {
    var primes = List[Int]()
    var notPrimes = Set[Int]()

    def isNotYetExcluded(n: Int) = !notPrimes.contains(n)

    for (potentialPrime <- 2 to max) {
      if (isNotYetExcluded(potentialPrime)) {
        primes = primes ++ List(potentialPrime)
        notPrimes = notPrimes ++ multiplesOf(potentialPrime, max)
      }
    }
    primes
  }

  private def multiplesOf(n: Int, upTo: Int) = {
    Stream.from(n, n).drop(1).takeWhile(_ <= upTo)
  }
}