import Bearing._

case class Robot(bearing: Bearing, coordinates: Point) {

  def turnRight: Robot = Robot(bearing.right, coordinates)

  def turnLeft: Robot = Robot(bearing.left, coordinates)

  def advance: Robot = bearing match {
    case North => Robot(bearing, coordinates.copy(y = coordinates.y + 1))
    case East => Robot(bearing, coordinates.copy(x = coordinates.x + 1))
    case South => Robot(bearing, coordinates.copy(y = coordinates.y - 1))
    case West => Robot(bearing, coordinates.copy(x = coordinates.x - 1))
  }

  def simulate(steps: String): Robot =
    steps.foldLeft(Robot(bearing, coordinates)) {
      case (robot, 'R') => robot.turnRight
      case (robot, 'L') => robot.turnLeft
      case (robot, 'A') => robot.advance
    }
}

object Robot {
  def apply(bearing: Bearing, coordinates: (Int, Int)): Robot =
    Robot(bearing, Point(coordinates._1, coordinates._2))
}

case class Point(x: Int, y: Int) extends Product2[Int, Int] {
  val _1: Int = x

  val _2: Int = y

  override def equals(that: Any): Boolean = that match {
    case Point(that_x, that_y) => that_x == x && that_y == y
    case (that_x, that_y) => that_x == x && that_y == y
    case _ => false
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