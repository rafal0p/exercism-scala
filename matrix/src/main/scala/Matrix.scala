case class Matrix(raw: String) {
  private val rawRows = raw.split('\n')
  private val rows = rawRows.map(row => row.split(' ').map(_.toInt).toVector)
  private val rowSize = rows.map(row => row.size).max
  private val cols = (0 until rowSize).map(i => rows.map(row => row(i)).toVector)

  def row(i: Int): Vector[Int] = rows(i)

  def column(i: Int): Vector[Int] = cols(i)
}
