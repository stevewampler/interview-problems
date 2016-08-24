package com.sgw.problems

/**
  * Calculates the square root of x using Newton's method.
  */
object CalcSqrt extends App {
  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess else sqrtIter(nextGuess(guess))

    def isGoodEnough(guess: Double): Boolean =
    ((guess * guess) - x).abs < x * 0.0001 // mult. error bounds by x so that small and large values of x are properly bounded

    def nextGuess(guess: Double): Double =
    (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  println(sqrt(2))
  println(sqrt(4))

  println(sqrt(1e-06))
  println(sqrt(1e60))

  println(sqrt(0.001))
  println(sqrt(0.1e-20))
  println(sqrt(1.0e20))
  println(sqrt(1.0e50))
}
