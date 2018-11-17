import scala.util.Random

class Robot {
  private var _name = AllNames.next

  def name: String = _name

  def reset(): Unit = {
    _name = AllNames.next
  }
}

object AllNames {
  private var unusedNames = Random.shuffle(generateAllNames.toList)

  def next: String = {
    val name = unusedNames.head
    unusedNames = unusedNames.tail
    name
  }

  private def generateAllNames = {
    allLetters
      .flatMap(withSecondLetter)
      .flatMap(withNumber)
  }

  private def allLetters = {
    Range.inclusive('A', 'Z').map(_.toChar)
  }

  private def withSecondLetter = {
    letter: Char => allLetters.map(newLetter => s"$letter$newLetter")
  }


  private def withNumber = {
    s: String => formattedNumbers.map(n => s"$s$n")
  }

  private def formattedNumbers = {
    Stream.range(0, 1000).map(_.formatted("%03d"))
  }
}