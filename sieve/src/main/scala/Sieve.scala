object Sieve {

  def primes(max: Int): Stream[Int] =
    sieve(Stream.from(2)).takeWhile(_ <= max)

  private def sieve(stream: Stream[Int]): Stream[Int] =
    stream.head #:: sieve(stream.tail.filter(_ % stream.head != 0))
}