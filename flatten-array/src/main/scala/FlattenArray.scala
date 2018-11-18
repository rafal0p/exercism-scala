object FlattenArray {
  def flatten(list: List[_]): List[_] = list match {
    case Nil => Nil
    case null :: t => flatten(t)
    case (h: List[_]) :: t => flatten(h) ::: flatten(t)
    case h :: t => h :: flatten(t)
  }
}