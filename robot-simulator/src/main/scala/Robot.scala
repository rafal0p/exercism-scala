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
  def left: Bearing
  def right: Bearing
}

case class NorthBearing() extends Bearing {
  override def left: Bearing = WestBearing()
  override def right: Bearing = EastBearing()
}

case class EastBearing() extends Bearing {
  override def left: Bearing = NorthBearing()
  override def right: Bearing = SouthBearing()
}

case class SouthBearing() extends Bearing {
  override def left: Bearing = EastBearing()
  override def right: Bearing = WestBearing()
}

case class WestBearing() extends Bearing {
  override def left: Bearing = SouthBearing()
  override def right: Bearing = NorthBearing()
}

object Bearing {
  val North: Bearing = NorthBearing()
  val East: Bearing = EastBearing()
  val South: Bearing = SouthBearing()
  val West: Bearing = WestBearing()
}