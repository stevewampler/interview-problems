package com.sgw.problems

/**
 * For the numbers between 1 and 100, print out either "Fizz" for those divisible by 3, "Buzz" for those divisible by 5
 * or "FizzBuzz" for those divisible by 3 and 5.
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
