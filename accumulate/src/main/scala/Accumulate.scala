import scala.annotation.tailrec

class Accumulate {
  def accumulate[A, B](f: A => B, list: List[A]): List[B] = {
    @tailrec
    def _accumulate(list: List[A], result: List[B]): List[B] =
      list match {
        case head :: tail => _accumulate(tail, f(head) :: result)
        case Nil => result.reverse
      }

    _accumulate(list, List())
  }
}