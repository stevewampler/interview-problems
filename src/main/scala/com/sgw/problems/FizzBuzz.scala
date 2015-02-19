package com.sgw.problems

/**
 * author: steve
 */
object FizzBuzz {
  def main (args: Array[String]) {
    (1 to 100)
      .map {
        case value if value % 3 == 0 && value % 5 == 0 => (value, "FizzBuzz")
        case value if value % 3 == 0 => (value, "Fizz")
        case value if value % 5 == 0 => (value, "Buzz")
        case value => (value, "")
    }
    .foreach(println)
  }
}
