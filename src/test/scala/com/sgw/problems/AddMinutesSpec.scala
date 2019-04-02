package com.sgw.problems

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.util.Success

class AddMinutesSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "toMinutesSinceMidnight" should "throw if a time string is invalid" in {
    AddMinutes.toMinutesSinceMidnight(timeStr =  "0:00 AM").isFailure should be (true)
    AddMinutes.toMinutesSinceMidnight(timeStr = "20:00 AM").isFailure should be (true)
    AddMinutes.toMinutesSinceMidnight(timeStr = "13:00 AM").isFailure should be (true)
    AddMinutes.toMinutesSinceMidnight(timeStr = "AA:00 AM").isFailure should be (true)
    AddMinutes.toMinutesSinceMidnight(timeStr = "12:60 AM").isFailure should be (true)
    AddMinutes.toMinutesSinceMidnight(timeStr = "13:00 PM").isFailure should be (true)
    AddMinutes.toMinutesSinceMidnight(timeStr =  "0:00 PM").isFailure should be (true)
    AddMinutes.toMinutesSinceMidnight(timeStr =  "1:00 QM").isFailure should be (true)
  }

  "toMinutesSinceMidnight" should "correctly parse valid time strings" in {
    AddMinutes.toMinutesSinceMidnight(timeStr = "12:00 AM") should be (Success(0))
    AddMinutes.toMinutesSinceMidnight(timeStr = "12:01 AM") should be (Success(1))
    AddMinutes.toMinutesSinceMidnight(timeStr = "12:59 AM") should be (Success(59))
    AddMinutes.toMinutesSinceMidnight(timeStr =  "1:00 AM") should be (Success(1 * 60))
    AddMinutes.toMinutesSinceMidnight(timeStr =  "9:00 AM") should be (Success(9 * 60))
    AddMinutes.toMinutesSinceMidnight(timeStr = "10:00 AM") should be (Success(10 * 60))
    AddMinutes.toMinutesSinceMidnight(timeStr = "11:59 AM") should be (Success(11 * 60 + 59))
    AddMinutes.toMinutesSinceMidnight(timeStr = "12:00 PM") should be (Success(12 * 60))
    AddMinutes.toMinutesSinceMidnight(timeStr = "12:01 PM") should be (Success(12 * 60 + 1))
    AddMinutes.toMinutesSinceMidnight(timeStr = "12:59 PM") should be (Success(12 * 60 + 59))
    AddMinutes.toMinutesSinceMidnight(timeStr = " 1:00 PM") should be (Success(13 * 60))
    AddMinutes.toMinutesSinceMidnight(timeStr = " 9:00 PM") should be (Success(21 * 60))
    AddMinutes.toMinutesSinceMidnight(timeStr = "11:00 PM") should be (Success(23 * 60))
    AddMinutes.toMinutesSinceMidnight(timeStr = "11:59 PM") should be (Success(23 * 60 + 59))
    AddMinutes.toMinutesSinceMidnight(timeStr = " 11:59  PM ") should be (Success(23 * 60 + 59))
    AddMinutes.toMinutesSinceMidnight(timeStr = " 1:59am ") should be (Success(1 * 60 + 59))
  }

  "fromMinutesSinceMidnight" should "generate a valid time string" in {
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = -(24 * 60 * 5 + 10)) should be (Success("11:50 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = -(24 * 60)) should be (Success("12:00 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = -(24 * 60) - 1) should be (Success("11:59 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = -(24 * 60) + 1) should be (Success("12:01 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = -47) should be (Success("11:13 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = -1) should be (Success("11:59 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 0) should be (Success("12:00 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 1) should be (Success("12:01 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 59) should be (Success("12:59 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 1 * 60) should be (Success("1:00 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 9 * 60) should be (Success("9:00 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 10 * 60) should be (Success("10:00 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 11 * 60 + 59) should be (Success("11:59 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 12 * 60) should be (Success("12:00 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 12 * 60 + 1) should be (Success("12:01 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 12 * 60 + 59) should be (Success("12:59 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 13 * 60) should be (Success("1:00 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 18 * 60) should be (Success("6:00 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 21 * 60) should be (Success("9:00 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 21 * 60 + 1) should be (Success("9:01 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 21 * 60 + 59) should be (Success("9:59 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 22 * 60) should be (Success("10:00 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 23 * 60) should be (Success("11:00 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 23 * 60 + 59) should be (Success("11:59 PM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 24 * 60) should be (Success("12:00 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 24 * 60 + 59) should be (Success("12:59 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 25 * 60) should be (Success("1:00 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 26 * 60) should be (Success("2:00 AM"))
    AddMinutes.fromMinutesSinceMidnight(minutesSinceMidnight = 24 * 60 * 5 + 10) should be (Success("12:10 AM"))
  }

  "AddMinutes when given various times and additional minutes" should "return the correct time" in {
    AddMinutes.addMinutes(timeStr =  "9:13 AM", minutes = 0) should be (Success( "9:13 AM")) // add 0 minutes
    AddMinutes.addMinutes(timeStr =  "9:13 AM", minutes = 1440) should be (Success( "9:13 AM")) // add 24 hours
    AddMinutes.addMinutes(timeStr =  "9:13 AM", minutes = -1440) should be (Success( "9:13 AM")) // sub 24 hours
    AddMinutes.addMinutes(timeStr =  "9:13 AM", minutes = 1440 * 2) should be (Success( "9:13 AM")) // add 48 hours
    AddMinutes.addMinutes(timeStr =  "9:13 AM", minutes = -1440 * 2) should be (Success( "9:13 AM")) // sub 48 hours
    AddMinutes.addMinutes(timeStr =  "9:13 AM", minutes = 200) should be (Success("12:33 PM")) // add 200 minutes
    AddMinutes.addMinutes(timeStr =  "9:13 AM", minutes = -200) should be (Success( "5:53 AM")) // sub 200 minutes
    AddMinutes.addMinutes(timeStr =  "9:13 AM", minutes = -13) should be (Success( "9:00 AM")) // to hour
    AddMinutes.addMinutes(timeStr = "11:13 AM", minutes = -13) should be (Success("11:00 AM")) // to hour, parse HH:MM
    AddMinutes.addMinutes(timeStr = "11:13 AM", minutes = 47) should be (Success("12:00 PM")) // to noon
    AddMinutes.addMinutes(timeStr =  "9:13 AM", minutes = -553) should be (Success("12:00 AM")) // to midnight
    AddMinutes.addMinutes(timeStr = "11:13 AM", minutes = 48) should be (Success("12:01 PM")) // beyond noon
    AddMinutes.addMinutes(timeStr =  "9:13 AM", minutes = -554) should be (Success("11:59 PM")) // short of midnight
    AddMinutes.addMinutes(timeStr =  "9:13 AM", minutes = 14*60+47) should be (Success("12:00 AM")) // midnight
  }
}
