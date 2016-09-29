package com.sgw.coursera

/**
  * Write a set of classes that can be used to evaluate an integer expression.
  *
  * Derived from the Coursera "Functional Programming Principles in Scala" course.
  *
  * See also the "Expression Problem":
  *   https://en.wikipedia.org/wiki/Expression_problem
  * and "Type Classes":
  *   https://en.wikipedia.org/wiki/Type_class
  */
object Expressions extends App {
  trait Expr {
    def eval: Int = this match {
      case Sum(x: Expr, y: Expr) => x.eval + y.eval
      case Prod(x: Expr, y: Expr) => x.eval * y.eval
      case Number(x) => x
      case Var(_) => 1 // TODO: ???
    }

    override def toString: String = this match {
      case Prod(s1: Sum, s2: Sum) => s"($s1) * ($s2)"
      case Prod(s: Sum, y: Expr) => s"($s) * $y"
      case Prod(x: Expr, s: Sum) => s"$x * ($s)"
      case Prod(x: Expr, y: Expr) => s"$x * $y"
      case Sum(x: Expr, y: Expr) => s"$x + $y"
      case Number(x) => s"$x"
      case Var(n) => n
    }
  }

  object Sum {
    def apply(x: Int, y: Int): Sum = Sum(Number(x), Number(y))
    def apply(x: Int, y: Expr): Sum = Sum(Number(x), y)
    def apply(x: Expr, y: Int): Sum = Sum(x, Number(y))

    def apply(x: String, y: String): Sum = Sum(Var(x), Var(y))
    def apply(x: String, y: Expr): Sum = Sum(Var(x), y)
    def apply(x: Expr, y: String): Sum = Sum(x, Var(y))
  }

  case class Sum(x: Expr, y: Expr) extends Expr

  object Prod {
    def apply(x: Int, y: Int): Prod = Prod(Number(x), Number(y))
    def apply(x: Int, y: Expr): Prod = Prod(Number(x), y)
    def apply(x: Expr, y: Int): Prod = Prod(x, Number(y))

    def apply(x: String, y: String): Prod = Prod(Var(x), Var(y))
    def apply(x: String, y: Expr): Prod = Prod(Var(x), y)
    def apply(x: Expr, y: String): Prod = Prod(x, Var(y))
  }

  case class Prod(x: Expr, y: Expr) extends Expr

  case class Number(x: Int) extends Expr

  case class Var(n: String) extends Expr

  val sumProd1 = Sum(Prod(2, 3), 5)

  println(s"$sumProd1 = ${sumProd1.eval}")

  val sumProd2 = Sum(2, Prod(3, 5))

  println(s"$sumProd2 = ${sumProd2.eval}")

  val sumProd3 = Sum(Prod(1, 2), Prod(3, 5))

  println(s"$sumProd3 = ${sumProd3.eval}")

  val prodSum1 = Prod(Sum(2, 3), 5)

  println(s"$prodSum1 = ${prodSum1.eval}")

  val prodSum2 = Prod(2, Sum(3, 5))

  println(s"$prodSum2 = ${prodSum2.eval}")

  val prodSum3 = Prod(Sum(1, 2), Sum(3, 5))

  println(s"$prodSum3 = ${prodSum3.eval}")

  // with variables:

  val eqSumProd1 = Sum(Prod("x", "y"), "z")

  println(s"$eqSumProd1 = ${eqSumProd1.eval}")

  val eqSumProd2 = Sum("x", Prod("y", "z"))

  println(s"$eqSumProd2 = ${eqSumProd2.eval}")

  val eqSumProd3 = Sum(Prod("a", "x"), Prod("y", "z"))

  println(s"$eqSumProd3 = ${eqSumProd3.eval}")

  val eqProdSum1 = Prod(Sum("x", "y"), "z")

  println(s"$eqProdSum1 = ${eqProdSum1.eval}")

  val eqProdSum2 = Prod("x", Sum("y", "z"))

  println(s"$eqProdSum2 = ${eqProdSum2.eval}")

  val eqProdSum3 = Prod(Sum("a", "x"), Sum("y", "z"))

  println(s"$eqProdSum3 = ${eqProdSum3.eval}")
}
