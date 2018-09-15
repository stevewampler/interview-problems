package com.sgw.problems

// Two problems:
// 1) What's the angle between the hour and minute hands of a clock at any time of day?
// 2) At what hour and minute combinations is the angle between the hour and minute hands zero?
object ClockHands {
  // what's the angle of the hour hand at a given hour and minute
  def hourAngle(hour: Int, minute: Double): Double = ((hour % 12) / 12.0 * 360) + (minute / 60.0 * 30.0)
  // what's the angle of the minute hand at a given minute
  def minuteAngle(minute: Double): Double = minute / 60.0 * 360.0
  // what's the difference between the angles of a minute and hour hand at a particular time
  def angleBetweenHandsAt(hour: Int, minute: Double): Double = minuteAngle(minute) - hourAngle(hour, minute)

  // given an hour, at what minute is are hour and minute hand at equal angles?
  def minuteFromHour(hour: Int): Double = {
//    hourAngle = minuteAngle
//    ((hour % 12) / 12.0 * 360) + (minute / 60.0 * 30) = minute / 60.0 * 360
//    ((hour % 12) / 12.0 * 360) + (minute / 60.0 * 30) - minute / 60.0 * 360 = 0
//    ((hour % 12) / 12.0 * 360) + minute / 2.0 - minute * 6.0
//    (hour * 30.0) + minute * ((1.0 / 2.0) - 6.0)
//    (hour * 30.0) - minute * 5.5
    30 * (hour % 12) / 5.5
  }

  def main(args: Array[String]): Unit = {
    for {
      hour <- 0 to 23
      minute <- 0 to 59
    } yield {
      println(s"$hour:$minute = ${angleBetweenHandsAt(hour, minute)}")
    }

    println("---------")
    for {
      hour <- 0 to 24
    } yield {
      val minute = minuteFromHour(hour)
      val angle = angleBetweenHandsAt(hour, minute)
      println(s"$hour:$minute $angle")
    }
  }
}
