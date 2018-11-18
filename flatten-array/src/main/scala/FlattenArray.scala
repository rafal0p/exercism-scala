object FlattenArray {
  def flatten(list: List[Any]): List[Int] = {
    def _flatten(list: List[Any], acc: List[Int]): List[Int] = list match {
      case head :: tail if head == null => _flatten(tail, acc)
      case (head: Int) :: tail => _flatten(tail, head :: acc)
      case (head: List[Int]) :: tail => _flatten(tail, _flatten(head, List()) ::: acc)
      case Nil => acc
    }

    _flatten(list, List()).reverse
  }
}