object Sieve {
  def primes(max: Int): List[Int] = {
    var numbers = (2 to max).map(i => (i, Option.empty[Boolean]))

    def markMultiplesOf(number: Int) = {
      numbers.map {
        case (i, _) if i == number => (i, Option(true))
        case (i, _) if multiplesOf(number, max).contains(i) => (i, Option(false))
        case (i, isEmpty) => (i, isEmpty)
      }
    }

    var n = findFirstUncheckedIn(numbers)
    while (n.nonEmpty) {
      numbers = markMultiplesOf(n.get)
      n = findFirstUncheckedIn(numbers)
    }

    numbers
      .filter { case (_, isPrime) => isPrime.getOrElse(false) }
      .map { case (i, _) => i }
      .toList
  }

  private def findFirstUncheckedIn(numbers: IndexedSeq[(Int, Option[Boolean])]) = {
    numbers find { case (_, isPrime) => isPrime.isEmpty } map { case (i, _) => i }
  }

  private def multiplesOf(n: Int, upTo: Int) = {
    Stream.from(n, n).drop(1).takeWhile(_ <= upTo)
  }
}