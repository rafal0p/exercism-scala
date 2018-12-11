import Bearing._

case class Robot(bearing: Bearing, coordinates: (Int, Int)) {

  def turnRight: Robot = Robot(bearing.right, coordinates)

  def turnLeft: Robot = Robot(bearing.left, coordinates)

  def advance: Robot = bearing match {
    case North => Robot(bearing, (coordinates._1, coordinates._2 + 1))
    case East => Robot(bearing, (coordinates._1 + 1, coordinates._2))
    case South => Robot(bearing, (coordinates._1, coordinates._2 - 1))
    case West => Robot(bearing, (coordinates._1 - 1, coordinates._2))
  }

  def simulate(steps: String): Robot =
    steps.foldLeft(Robot(bearing, coordinates)) {
      case (robot, 'R') => robot.turnRight
      case (robot, 'L') => robot.turnLeft
      case (robot, 'A') => robot.advance
    }
}

sealed trait Bearing {
  val left: Bearing
  val right: Bearing
}

object Bearing {

  case object North extends Bearing {
    val left: Bearing = West
    val right: Bearing = East
  }

  case object East extends Bearing {
    val left: Bearing = North
    val right: Bearing = South
  }

  case object South extends Bearing {
    val left: Bearing = East
    val right: Bearing = West
  }

  case object West extends Bearing {
    val left: Bearing = South
    val right: Bearing = North
  }

}