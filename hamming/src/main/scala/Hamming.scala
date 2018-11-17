import scala.annotation.tailrec

object Hamming {

  def distance(left: String, right: String): Option[Int] = {
    countDistance(left.toList, right.toList, Some(0))
  }

  @tailrec
  private def countDistance(left: List[Char],
                            right: List[Char],
                            acc: Option[Int]): Option[Int] = (left, right) match {
    case (Nil, Nil) => acc
    case (Nil, _) => None
    case (_, Nil) => None
    case (lh :: lt, rh :: rt) =>
      if (lh != rh)
        countDistance(lt, rt, acc.map(_ + 1))
      else
        countDistance(lt, rt, acc)
  }
}