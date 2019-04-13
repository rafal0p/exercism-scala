import scala.util.Random

object BookStore {
  def total(books: List[Int]): Double = {
    total(books.foldLeft(Map[Int, Group]())(insertBookInBestGroup))
  }

  private def insertBookInBestGroup(groups: Map[Int, Group], book: Int): Map[Int, Group] = {
    val potentialGroups = groups.filterNot { case (_, group) => group.contains(book) }

    if (potentialGroups.isEmpty) {
      groups + (Random.nextInt() -> Group(Set(book)))
    } else {
      val bestGroupId = potentialGroups
        .map { case (id, group) => (total(groups.updated(id, group.add(book))), id) }
        .minBy { case (total, _) => total }
        ._2

      groups.updated(bestGroupId, groups(bestGroupId).add(book))
    }
  }

  private def total(groups: Map[Int, Group]) = groups.values.map(_.price).sum

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
