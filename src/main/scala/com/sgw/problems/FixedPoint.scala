package com.sgw.problems

/**
  * A "fixed point" of a function is the value x such that f(x) = x.
  *
  * For some functions we can find the fixed point by iteratively applying x, f(x), f(f(x)), f(f(f(x))), until
  * the result no longer changes (or doesn't change much).
  *
  * The sqrt function is defined such that;
  *
  *  sqrt(x) = the number y such that y * y = x
  *
  * so y * y is the fixed point of the sqrt function.
  *
  * If we divide by y, then:
  *
  *  sqrt(x) = the number y such that y = x / y
  *
  * So:
  *
  *  sqrt(x) is a fixed point of the equation y = x / y.
  */
object FixedPoint extends App{
  def line(x: Double): Double = 1 + x / 2.0

  // a function whose fixed point is the sqrt(x)
  def xOverY(x: Double)(y: Double): Double = x / y // oscillates in the fixedPoints function and doesn't converge, so ...

  // a function that iteratively finds the fixed point of another function
  def fixedPoint(f: (Double) => Double)(guess: Double, tolerance: Double = 0.0001): Double = {
    def isCloseEnough(x: Double, y: Double): Boolean = (x - y).abs / x < tolerance

    def iter(x: Double): Double = {
      val y = f(x)

      println(s"x=$x, y=$y")

      if (isCloseEnough(x, y)) y
      else iter(y)
    }

    iter(guess)
  }

  println("-" * 50)
  println(fixedPoint(line)(5))
  assert((fixedPoint(line)(5) - 2.0).abs < 0.001)

  println("-" * 50)
  // println(fixedPoint(xOverY(4))(5)) // the sqrt(4), oscillates, so ...

  // a damping function to help the xOverY function converge
  def avgDamp(f: (Double) => Double)(x: Double) = (x + f(x)) / 2.0

  println(fixedPoint(avgDamp(xOverY(4)))(5)) // damp
  assert((fixedPoint(avgDamp(xOverY(4)))(5) - 2.0).abs < 0.001)

  println("-" * 50)
  def sqrt(x: Double): Double = fixedPoint(avgDamp(xOverY(x)))(x)

  println(sqrt(4))
  assert((fixedPoint(avgDamp(xOverY(4)))(5) - 2.0).abs < 0.001)
}
