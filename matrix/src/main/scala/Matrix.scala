case class Matrix(raw: String) {
  private lazy val rawRows = raw.split('\n')
  private lazy val rows = rawRows.map(row => row.split(' ').map(_.toInt).toVector)
  private lazy val rowSize = rows.map(row => row.size).max
  private lazy val cols = (0 until rowSize).map(i => rows.map(row => row(i)).toVector)

  def row(i: Int): Vector[Int] = rows(i)

  def column(i: Int): Vector[Int] = cols(i)
}
