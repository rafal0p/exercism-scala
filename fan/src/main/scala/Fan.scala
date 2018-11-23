object Fan {
  def generate(size: Int): String = {
    val bigSize = 2 * size - 1
    val on = '*'
    val off = '.'

    def shouldFill(x: Int, y: Int) = {
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

    def charToPrint(x: Int, y: Int) = {
      if (shouldFill(x, y)) on else off
    }

    plane(2 * size)
      .map { case (x, y) => (x, y, charToPrint(x, y)) }
      .groupBy { case (_, y, _) => y }
      .toSeq.sortBy { case (y, _) => -y }
      .map { case (_, line) => line
        .sortBy { case (x, _, _) => x }
        .map { case (_, _, c) => c }
        .mkString
      }
      .mkString("\n")
  }

  private def plane(size: Int) =
    (0 until size).flatMap { x =>
      (0 until size).map { y =>
        (x, y)
      }
    }
}
