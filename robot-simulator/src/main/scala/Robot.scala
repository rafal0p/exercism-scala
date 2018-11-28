import Bearing.Bearing

case class Robot(bearing: Bearing, coordinates: (Int, Int)) {
  def turnRight: Robot = bearing match {
    case Bearing.North => Robot(Bearing.East, coordinates)
    case Bearing.East => Robot(Bearing.South, coordinates)
    case Bearing.South => Robot(Bearing.West, coordinates)
    case Bearing.West => Robot(Bearing.North, coordinates)
  }

  def turnLeft: Robot = bearing match {
    case Bearing.North => Robot(Bearing.West, coordinates)
    case Bearing.East => Robot(Bearing.North, coordinates)
    case Bearing.South => Robot(Bearing.East, coordinates)
    case Bearing.West => Robot(Bearing.South, coordinates)
  }

  def advance: Robot = bearing match {
    case Bearing.North => Robot(Bearing.North, (coordinates._1, coordinates._2 + 1))
    case Bearing.East => Robot(Bearing.East, (coordinates._1 + 1, coordinates._2))
    case Bearing.South => Robot(Bearing.South, (coordinates._1, coordinates._2 - 1))
    case Bearing.West => Robot(Bearing.West, (coordinates._1 - 1, coordinates._2))
  }


  def simulate(steps: String): Robot =
    steps.foldLeft(Robot(bearing, coordinates)) {
      case (robot, 'R') => robot.turnRight
      case (robot, 'L') => robot.turnLeft
      case (robot, 'A') => robot.advance
    }
}

object Bearing extends Enumeration {
  type Bearing = Value
  val North, East, South, West = Value
}