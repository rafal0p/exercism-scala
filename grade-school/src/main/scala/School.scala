import scala.collection.immutable.{SortedMap, TreeMap}

class School {
  type DB = Map[Int, Seq[String]]

  private var state = SortedMap[Int, Seq[String]]()

  def add(name: String, g: Int): Unit = {
    val existingStudents = state.applyOrElse(g, (_: Int) => Seq())
    state += (g -> (existingStudents ++ Seq(name)))
  }

  def db: DB = state

  def grade(g: Int): Seq[String] = state.applyOrElse(g, (_: Int) => Seq())

  def sorted: DB = state.mapValues(_.sorted)
}