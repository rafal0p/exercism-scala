object SecretHandshake {
  def commands(code: Int): List[String] =
    if (isOn(bitPos = 4, in = code))
      actionsIn(code).reverse
    else
      actionsIn(code)

  private def isOn(bitPos: Int, in: Int) = (in & (1 << bitPos)) != 0

  private def actionsIn(code: Int) =
    availableActions
      .filter { case (key, _) => isOn(bitPos = key, in = code) }
      .values
      .toList

  private val availableActions: Map[Int, String] = Map(
    0 -> "wink",
    1 -> "double blink",
    2 -> "close your eyes",
    3 -> "jump"
  )
}
