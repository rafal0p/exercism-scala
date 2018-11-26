object SecretHandshake {
  def commands(code: Int): List[String] = {
    var result = List[String]()

    def isOn(bit: Int) = {
      (code & bit) == bit
    }

    if (isOn(bit = 8))
      result = "jump" :: result
    if (isOn(bit = 4))
      result = "close your eyes" :: result
    if (isOn(bit = 2))
      result = "double blink" :: result
    if (isOn(bit = 1))
      result = "wink" :: result
    if (isOn(bit = 16))
      result = result.reverse

    result
  }
}
