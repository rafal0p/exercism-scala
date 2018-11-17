import scala.collection.immutable.SortedMap

class School {
  type DB = Map[Int, Seq[String]]

  private var state = SortedMap[Int, Seq[String]]()

  def add(name: String, g: Int): Unit = {
    state += (g -> (grade(g) ++ Seq(name)))
  }

  def db: DB = state

  def grade(g: Int): Seq[String] = state.applyOrElse(g, (_: Int) => Seq())

  def sorted: DB = state.mapValues(_.sorted)
}