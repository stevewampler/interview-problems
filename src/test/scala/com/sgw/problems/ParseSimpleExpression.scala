package com.sgw.problems

object ParseSimpleExpression {

  // this method solves a simple integer expression containing only integers and + and -
  def solve(str: String): Int = {
    val nums: List[Int] = str.split("[+-]").toList.filter(_.nonEmpty).map(_.trim).map(_.toInt)
    val ops = '+' :: str.filter(c => c == '+' || c == '-').toList

    println(nums)
    println(ops)

    nums.zip(ops).foldLeft(0) { case (acc, (num, op)) =>
      op match {
        case '+' => acc + num
        case '-' => acc - num
        case ' ' => acc
      }
    }
  }

  def applyOp(op: Char, num: Int): Int = op match {
    case '+' => num
    case '-' => -num
  }

  // this one solve an expression that contains +, -, (, and ) characters
  // assume all parenthesis are balanced
  def solve2(str: String, i: Int = 0): (Int, Int) = {
    var j = i
    var lastOp = '+'
    var num = 0
    var result = 0
    var done = false

//    println(str)

    while (j < str.length && !done) {
      val c = str(j)

//      println(s"before: j=$j, str($j)=${str(j)}, num=$num, result=$result, lastOp=$lastOp, done=$done")

      c match {
        case '+' =>
          result = result + applyOp(lastOp, num)
          num = 0
          lastOp = '+'
          j = j + 1
        case '-' =>
          result = result + applyOp(lastOp, num)
          num = 0
          lastOp = '-'
          j = j + 1
        case '(' =>
          val (newNum, newJ) = solve2(str, j + 1)
          result = result + applyOp(lastOp, newNum)
          num = 0
          j = newJ
        case ')' =>
          result = result + applyOp(lastOp, num)
          num = 0
          j = j + 1
          done = true
        case ' ' =>
          j = j + 1
        case c if c.isDigit =>
          num = num * 10 + (c - '0')
          j = j + 1
        case c: Char => throw new RuntimeException(s"Unexpected character '$c' in expression $str")
      }

//      println(s"after:  j=$j, num=$num, result=$result, lastOp=$lastOp, done=$done")
    }

    (result + applyOp(lastOp, num), j)
  }

  def checkResult(actual: Int, expected: Int): Unit = {
    println(s"actual=$actual, expected=$expected")
    assert(actual == expected)
  }

  def main(args: Array[String]): Unit = {
    checkResult(solve("10+5+0-4"), 11)
    checkResult(solve2("10+5+0-4")._1, 11)
    checkResult(solve2("10+5+(0-4)")._1, 11)
    checkResult(solve2("10+5-(1-4)")._1, 18)
    checkResult(solve2("10+5-1-4")._1, 10)
    checkResult(solve2("-(10+5)-(1-4)")._1, -12)
  }
}
