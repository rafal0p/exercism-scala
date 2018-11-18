object FlattenArray {
  def flatten(list: List[Any]): List[Int] =
    list.flatMap {
      case elem if elem == null => List()
      case elem: Int => List(elem)
      case elem: List[Any] => flatten(elem)
    }
}