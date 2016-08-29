package com.sgw.problems

/**
  * 0) Write a sum function that calculates the sum of the values of a function for the points on a given interval.
  * 1) Write a product function that calculates the product of the values of a function for the points on a given interval.
  * 2) Write factorial in terms of product.
  * 3) Write a function that generalizes both product and sum.
  */
object Currying extends App {
  def sum(f: (Int) => Int)(a: Int, b: Int) = {
    def iter(a: Int, acc: Int): Int =
      if (a > b) acc
      else iter(a + 1, f(a) + acc)

    iter(a, 0)
  }

  assert(sum((x: Int) => x)(0, 5) == 15)
  assert(sum((x: Int) => x * x)(0, 5) == 55)

  def prod(f: (Int) => Int)(a: Int, b: Int) = {
    def iter(a: Int, acc: Int): Int =
      if (a > b) acc
      else iter(a + 1, f(a) * acc)

    iter(a, 1)
  }

  assert(prod((x: Int) => x)(1, 5) == 120)
  assert(prod((x: Int) => x * x)(1, 5) == 14400)

  def fact(n: Int): Int = prod((x: Int) => x)(1, n)

  assert(fact(5) == 120)

  def gen(op: (Int, Int) => Int)(zero: Int)(f: (Int) => Int)(a: Int, b: Int) = {
    def iter(a: Int, acc: Int): Int =
      if (a > b) acc
      else iter(a + 1, op(f(a), acc))

    iter(a, zero)
  }

  val prod2 = gen((x1: Int, x2: Int) => x1 * x2)(1) _

  assert(prod2((x: Int) => x)(1, 5) == 120)

  val sum2 = gen((x1: Int, x2: Int) => x1 + x2)(0) _

  assert(sum2((x: Int) => x * x)(0, 5) == 55)
}
