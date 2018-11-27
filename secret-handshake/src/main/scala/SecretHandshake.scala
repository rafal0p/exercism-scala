object SecretHandshake {
  def commands(code: Int): List[String] =
    if (isOn(bit = 16, in = code))
      actionsIn(code).reverse
    else
      actionsIn(code)

  private def isOn(bit: Int, in: Int) = (in & bit) == bit

  private def actionsIn(code: Int) =
    availableActions
      .filter { case (key, _) => isOn(bit = key, in = code) }
      .values
      .toList

  private val availableActions: Map[Int, String] = Map(
    1 -> "wink",
    2 -> "double blink",
    4 -> "close your eyes",
    8 -> "jump"
  )
}
