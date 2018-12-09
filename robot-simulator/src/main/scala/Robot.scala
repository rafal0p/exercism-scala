case class Robot(bearing: Bearing, coordinates: (Int, Int)) {

  def turnRight: Robot = Robot(bearing.right, coordinates)

  def turnLeft: Robot = Robot(bearing.left, coordinates)

  def advance: Robot = bearing match {
    case NorthBearing() => Robot(bearing, (coordinates._1, coordinates._2 + 1))
    case EastBearing() => Robot(bearing, (coordinates._1 + 1, coordinates._2))
    case SouthBearing() => Robot(bearing, (coordinates._1, coordinates._2 - 1))
    case WestBearing() => Robot(bearing, (coordinates._1 - 1, coordinates._2))
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

case class NorthBearing() extends Bearing {
  override lazy val left: Bearing = WestBearing()
  override lazy val right: Bearing = EastBearing()
}

case class EastBearing() extends Bearing {
  override lazy val left: Bearing = NorthBearing()
  override lazy val right: Bearing = SouthBearing()
}

case class SouthBearing() extends Bearing {
  override lazy val left: Bearing = EastBearing()
  override lazy val right: Bearing = WestBearing()
}

case class WestBearing() extends Bearing {
  override lazy val left: Bearing = SouthBearing()
  override lazy val right: Bearing = NorthBearing()
}

object Bearing {
  val North: Bearing = NorthBearing()
  val East: Bearing = EastBearing()
  val South: Bearing = SouthBearing()
  val West: Bearing = WestBearing()
}