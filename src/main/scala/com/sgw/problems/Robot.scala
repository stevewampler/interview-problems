package com.sgw.problems

/**
 * A robot starting at some position on a grid and pointed in some direction,
 * excepts a list of commands as a string where each character in the
 * string represents a command to either go forward one step ('G'), turn right ('R'), or turn left ('L').
 * Assuming the robot
 * is initially at position (0,0) facing North. Return true if the robot ends up in a specified circle of radius R.
 */
object Robot {
  private val moveX = Array[Long](0, 1, 0,-1)
  private val moveY = Array[Long](1, 0,-1, 0)
}

case class Robot(posX: Long = 0, posY: Long = 0, dir: Int = 0) {
  import Robot._

  def translate(x: Long, y: Long): Robot = copy(posX + x, posY + y)
  def moveForward: Robot = translate(moveX(dir), moveY(dir))
  def turnLeft: Robot = copy(dir = (dir + 3) % 4)
  def turnRight: Robot = copy(dir = (dir + 1) % 4)

  def move(cmds: String): Robot = cmds.foldLeft(this) {
    case (robot, 'G') => robot.moveForward
    case (robot, 'L') => robot.turnLeft
    case (robot, 'R') => robot.turnRight
  }

  def distanceFromOrigin: Double = Math.sqrt(posX * posX + posY * posY)

  def isInCircle(centerX: Long, centerY: Long, radius: Double): Boolean =
    translate(-centerX, -centerY).distanceFromOrigin <= radius
}