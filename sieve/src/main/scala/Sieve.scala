object Sieve {

  def primes(max: Int): List[Int] = {
    findPrimes(List(), (2 to max).toList)
  }

  private def findPrimes(foundSoFar: List[Int], list: List[Int]): List[Int] = {
    if (list.nonEmpty) {
      val head = list.head
      val tail = list.tail
      findPrimes(head :: foundSoFar, withoutMultiples(of = head, from = tail))
    } else {
      foundSoFar.reverse
    }
  }

  private def withoutMultiples(of: Int, from: List[Int]) =
    from.filter(i => i % of != 0)
}