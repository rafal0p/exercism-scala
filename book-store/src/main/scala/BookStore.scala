object BookStore {
  def total(books: List[Int]): Double = {
    total(books.foldLeft(Seq[Group]())(insertBookInBestGroup))
  }

  private def insertBookInBestGroup(groups: Seq[Group], book: Int): Seq[Group] = {
    val potentialGroups = groups
      .zipWithIndex
      .filterNot { case (group, _) => group.contains(book) }

    if (potentialGroups.isEmpty) {
      groups :+ Group(Set(book))
    } else {
      val idxOfBestGroup = potentialGroups
        .map { case (group, idx) => (total(groups.updated(idx, group.add(book))), idx) }
        .minBy { case (total, _) => total }
        ._2

      groups.updated(idxOfBestGroup, groups(idxOfBestGroup).add(book))
    }
  }

  private def total(groups: Seq[Group]) = groups.map(_.price).sum

  private def discountAmountFor(numberOfBooks: Int) = numberOfBooks * discountRateFor(numberOfBooks) * basePrice

  private val discountRateFor = Map(
    0 -> 0.00,
    1 -> 0.00,
    2 -> 0.05,
    3 -> 0.10,
    4 -> 0.20,
    5 -> 0.25
  )

  private val basePrice = 800

  case class Group(books: Set[Int]) {
    val price: Double = books.size * basePrice - discountAmountFor(books.size)

    def add(book: Int): Group = Group(books + book)

    def contains(book: Int): Boolean = books.contains(book)
  }

}
