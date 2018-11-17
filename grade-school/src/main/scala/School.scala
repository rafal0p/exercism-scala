import scala.collection.immutable.SortedMap

class School {
  type DB = Map[Int, Seq[String]]

  private var state = SortedMap[Int, Seq[String]]().withDefaultValue(Seq())

  def add(name: String, g: Int): Unit = {
    state += (g -> (state(g) :+ name))
  }

  def db: DB = state

  def grade(g: Int): Seq[String] = state(g)

  def sorted: DB = state.mapValues(_.sorted)
}