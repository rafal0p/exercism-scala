case class Matrix(input: String) {
  private lazy val rows = input.lines.map(lineToInts).toVector
  private lazy val cols = rows.transpose

  def row(i: Int): Vector[Int] = rows(i)

  def column(i: Int): Vector[Int] = cols(i)

  private def lineToInts(line: String) = {
    line.split(' ').map(_.toInt).toVector
  }
}
