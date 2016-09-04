package com.sgw.problems

/**
  * Can all of the Scala primitive types be implemented as objects without resorting to the scala or java primitive types?
  *
  * From the Coursera Functional Programming in Scala course.
  */
abstract class MyBoolean {
  def ifThenElse[T](t: => T, e: => T): T

  def &&(x: MyBoolean): MyBoolean = ifThenElse(x, myFalse)
  def ||(x: MyBoolean): MyBoolean = ifThenElse(myTrue, x)
  def unary_! : MyBoolean = ifThenElse(myFalse, myTrue)

  def ==(x: MyBoolean): MyBoolean = ifThenElse(x, !x)
  def !=(x: MyBoolean): MyBoolean = ifThenElse(!x, x)

  // where false < true
  // if the current boolean is true ...
  //   and x is false => false
  //   and x is true  => ?
  // if the current boolean is false ...
  //   and x is false => ?
  //   and x is true => true == x
  def <(x: MyBoolean): MyBoolean = ifThenElse(myFalse, x)

  // similar for other inequalities
}

object myTrue extends MyBoolean {
  def ifThenElse[T](t: => T, e: => T): T = t
}

object myFalse extends MyBoolean {
  def ifThenElse[T](t: => T, e: => T): T = e
}