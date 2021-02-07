package com.sgw.utils

import scala.util.Try

/**
 * A simple class used to time how long it takes to execute a specified function.
 */
object Timer {
  def apply[R](f: => R): Try[(Time, R)] = Try {
    val startTime = Time.now
    val result: R = f
    (startTime.age, result)
  }
}
