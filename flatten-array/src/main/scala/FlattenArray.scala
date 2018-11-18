object FlattenArray {
  def flatten(list: List[Any]): List[Int] =
    list.flatten((value: Any) => value match {
      case x if x == null => List()
      case v: Int => List(v)
      case list: List[Int] => flatten(list)
    })

}
