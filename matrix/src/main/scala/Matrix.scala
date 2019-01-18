case class Matrix(raw: String) {
  private lazy val rawRows = raw.split('\n').toVector
  private lazy val rows = rawRows.map(row => row.split(' ').map(_.toInt).toVector)
  private lazy val cols = rows.transpose

  def row(i: Int): Vector[Int] = rows(i)

  def column(i: Int): Vector[Int] = cols(i)
}
