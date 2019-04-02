package com.sgw.problems

import scala.util.Try

/**
  * Without using any built-in date or time functions, write a function or method that accepts two
  * mandatory arguments: the first argument is a 12-hour time string with the format "[H]H:MM
  * {AM|PM}", and the second argument is a (signed) integer. The second argument is the number of
  * minutes to add to the time of day represented by the first argument. The return value should be a
  * string of the same format as the first argument. For example, AddMinutes("9:13 AM", 200) would
  * return "12:33 PM".
  */
object AddMinutes {
  private val hoursPerDay = 24
  private val minutesPerHour = 60
  private val minutesPerDay = hoursPerDay * minutesPerHour
  private val timeRegex = """([1-9]|1[0-2]):([0-5][0-9])\s*([AP]M)""".r

  /**
    * Converts the specified hour (between 1 and 12 inclusive) and an "AM/PM" indicator to a hour between
    * 0 and 23 inclusive.
    *
    * @param hour12 a hour between 1 and 12 inclusive
    * @param amOrPM the string "AM" or "PM"
    */
  def toHour24(hour12: Int, amOrPM: String): Int = {
    assert(hour12 >= 1 && hour12 <= 12)
    assert(amOrPM == "AM" || amOrPM == "PM")

    val hour24 = if (amOrPM == "AM") {
      if (hour12 == 12) {
        0
      } else {
        hour12
      }
    } else {
      if (hour12 == 12) {
        hour12
      } else {
        hour12 + 12
      }
    }

    assert(hour24 >= 0 && hour24 < 24)

    hour24
  }

  /**
    * Parses the specified time string and returns the number of minutes since midnight.
    *
    * @param timeStr a time string of the form "[H]H:MM {AM|PM}"
    */
  def toMinutesSinceMidnight(timeStr: String): Try[Int] = Try {
    timeStr.trim.toUpperCase match {
      case timeRegex(hour, minute, amOrPM) => (hour.toInt, minute.toInt, amOrPM)
    }
  }.map { case (hour, minute, amOrPM) =>
    (toHour24(hour, amOrPM), minute)
  }.map { case (hour, minute) =>
    hour * 60 + minute
  }

  /**
    * Returns the string "AM" or "PM" given an hour between 0 and 23 inclusive.
    *
    * @param hour24 the number of hours between 0 and 23 inclusive
    */
  def toAMOrPM(hour24: Int): String = {
    assert(hour24 >= 0 && hour24 < 24)

    if (hour24 >= 0 && hour24 < 12) {
      "AM"
    } else {
      "PM"
    }
  }

  /**
    * Converts the specified number of minutes since midnight into a time string of the form "[H]H:MM {AM|PM}".
    *
    * @param minutesSinceMidnight a integer representing the number of minutes since midnight (can be negative)
    */
  def fromMinutesSinceMidnight(minutesSinceMidnight: Int): Try[String] = Try {
    // between 0 and 1439
    val minutesIn24Hours = if (minutesSinceMidnight < 0) {
      (minutesSinceMidnight % minutesPerDay + minutesPerDay) % minutesPerDay
    } else {
      minutesSinceMidnight % minutesPerDay
    }

    assert(minutesIn24Hours >= 0 && minutesIn24Hours < 1440)

    // between 0 and 23
    val hoursIn24Hours = (minutesIn24Hours / minutesPerHour) % 24

    assert(hoursIn24Hours >= 0 && hoursIn24Hours < 24)

    // between 0 and 11
    val hoursIn12Hours = (minutesIn24Hours / minutesPerHour) % 12

    assert(hoursIn12Hours >= 0 && hoursIn12Hours < 12)

    // between 1 and 12
    val hour12 = if (hoursIn12Hours == 0) {
      12
    } else {
      hoursIn12Hours
    }

    assert(hour12 >= 1 && hour12 <= 12)

    // between 0 and 59
    val minute = minutesIn24Hours % minutesPerHour

    assert(minute >= 0 && minute < 60)

    f"$hour12:$minute%02d ${toAMOrPM(hoursIn24Hours)}"
  }

  /**
    * Adds the specified number of minutes to the specified time string of the form "[H]H:MM {AM|PM}" and returns a
    * new time string with the additional minutes added also in the form "[H]H:MM {AM|PM}".
    *
    * @param timeStr a time string of the form "[H]H:MM {AM|PM}"
    * @param minutes the number of minutes to add. Can be positive, negative, or zero.
    */
  def addMinutes(timeStr: String, minutes: Int): Try[String] = {
    toMinutesSinceMidnight(timeStr).map { minutesSinceMidnight =>
      minutesSinceMidnight + minutes
    }.flatMap(fromMinutesSinceMidnight)
  }
}
