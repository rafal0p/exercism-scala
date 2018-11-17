object BeerSong {

  def recite(startBottle: Int, versesNumber: Int): String =
    Stream
      .from(startBottle, -1)
      .take(versesNumber)
      .map(generateVerse)
      .mkString("\n")

  private def generateVerse(bottle: Int) = {
    bottle match {
      case 0 => noBottlesVerse
      case 1 => oneBottleVerse
      case 2 => twoBottlesVerse
      case _ => genericVerse(bottle)
    }
  }

  private val noBottlesVerse = "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"

  private val oneBottleVerse = "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n"

  private val twoBottlesVerse = "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall.\n"

  private def genericVerse(bottle: Int) = s"$bottle bottles of beer on the wall, $bottle bottles of beer.\nTake one down and pass it around, ${bottle - 1} bottles of beer on the wall.\n"
}
