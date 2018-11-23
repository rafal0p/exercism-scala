object Fan {
  def generate(size: Int): String = {
    val bigSize = 2 * size - 1
    val on = '*'
    val off = '.'

    def shouldFill(point: Point) = point match {
      case Point(x, y) =>
        if (x < size && y < size && y >= x)
          true
        else if (x < size && y >= size && y >= -x + bigSize)
          true
        else if (x >= size && y >= size && y <= x)
          true
        else if (x >= size && y < size && y <= -x + bigSize)
          true
        else
          false
    }

    def charToPrint(point: Point) = {
      if (shouldFill(point)) on else off
    }

    plane(2 * size)
      .map(point => PrintedPoint(point.x, point.y, charToPrint(point)))
      .groupBy(_.y)
      .toSeq.sortBy { case (y, _) => -y }
      .map(_._2)
      .map(line => line.sortBy(_.x).map(_.c).mkString)
      .mkString("\n")
  }

  private def plane(size: Int) =
    (0 until size).flatMap { x =>
      (0 until size).map { y =>
        Point(x, y)
      }
    }

  case class Point(x: Int, y: Int)

  case class PrintedPoint(x: Int, y: Int, c: Char)

}
