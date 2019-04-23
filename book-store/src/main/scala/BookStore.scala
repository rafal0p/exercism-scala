import scala.util.Random

object BookStore {
  def total(books: List[Int]): Double = {
    total(books.foldLeft(Set[Group]())(insertBookInBestGroup))
  }

  private def insertBookInBestGroup(groups: Set[Group], book: Int): Set[Group] = {
    val potentialGroups = groups.filterNot(_.contains(book))

    if (potentialGroups.isEmpty) {
      groups + new Group(Set(book))
    } else {
      val bestGroup = potentialGroups
        .map(group => (group, total(groups - group + group.add(book))))
        .minBy { case (_, total) => total }
        ._1

      groups - bestGroup + bestGroup.add(book)
    }
  }

  private def total(groups: Set[Group]) = groups.toSeq.map(_.price).sum

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

  class Group(books: Set[Int]) {
    val price: Double = books.size * basePrice - discountAmountFor(books.size)

    def add(book: Int): Group = new Group(books + book)

    def contains(book: Int): Boolean = books.contains(book)
  }

}
