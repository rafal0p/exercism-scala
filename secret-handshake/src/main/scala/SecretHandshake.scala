object SecretHandshake {
  def commands(code: Int): List[String] =
    actions
      .filter { case (key, _) => isOn(bitPos = key, in = code) }
      .values
      .foldLeft(List[String]())((acc, apply) => apply(acc))
      .reverse

  private def isOn(bitPos: Int, in: Int) = (in & (1 << bitPos)) != 0

  private val actions: Map[Int, List[String] => List[String]] = Map(
    0 -> (list => "wink" :: list),
    1 -> (list => "double blink" :: list),
    2 -> (list => "close your eyes" :: list),
    3 -> (list => "jump" :: list),
    4 -> (list => list.reverse)
  )
}
