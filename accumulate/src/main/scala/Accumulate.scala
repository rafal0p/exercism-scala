class Accumulate {
  def accumulate[A, B](f: A => B, list : List[A]): List[B] =
    list match {
      case head :: tail => f(head) :: accumulate(f, tail)
      case _ => List[B]()
    }
}