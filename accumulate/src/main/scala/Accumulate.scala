import scala.annotation.tailrec

class Accumulate {
  def accumulate[A, B](f: A => B, list: List[A]): List[B] =
    _accumulate(f, list, List[B]())

  @tailrec
  private def _accumulate[A, B](f: A => B, list: List[A], result: List[B]): List[B] =
    list match {
      case head :: tail => _accumulate(f, tail, result :+ f(head))
      case _ => result
    }
}