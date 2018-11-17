object Sieve {
  def primes(max: Int): List[Int] = {
    val numbers = (0 to max).map(_ => Option.empty[Boolean]).toArray
    var primes = List[Int]()
    numbers(0) = Option(false)
    numbers(1) = Option(false)

    (0 to max).foreach(i => {
      if (numbers(i).isEmpty) {
        numbers(i) = Option(true)
        primes = primes :+ i
        multiplesOf(i, max).foreach(i => numbers(i) = Option(false))
      }
    })

    primes
  }

  private def multiplesOf(n: Int, upTo: Int) = {
    Stream.from(n, n).drop(1).takeWhile(_ <= upTo)
  }
}