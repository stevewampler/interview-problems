package com.sgw.hackerrank.stacks

import scala.util.Try
import scala.collection._

object BalancedBrackets {

  private def popAndExpect(stack: mutable.Stack[Char], expected: Char): mutable.Stack[Char] = {
    if (stack.isEmpty) {
      throw new RuntimeException("Empty stack!")
    }

    val actual = stack.pop

    if (actual == expected) {
      stack
    } else {
      throw new RuntimeException(s"Expected a $expected, but popped a $actual")
    }
  }

  // Complete the isBalanced function below.
  def isBalanced(s: String): String = Try {
    val stack = s.foldLeft(mutable.Stack[Char]()) { case (stack, c) =>
      c match {
        case '{' => stack.push(c)
        case '}' => popAndExpect(stack, '{')
        case '[' => stack.push(c)
        case ']' => popAndExpect(stack, '[')
        case '(' => stack.push(c)
        case ')' => popAndExpect(stack, '(')
        case _ => stack
      }
    }

    if (stack.isEmpty) {
      "YES"
    } else {
      "NO"
    }
  }.getOrElse("NO")

  def main(args: Array[String]) {
    assert(isBalanced("") == "YES")
    assert(isBalanced("abcde") == "YES")
    assert(isBalanced("[") == "NO")
    assert(isBalanced("}") == "NO")
    assert(isBalanced("()") == "YES")
    assert(isBalanced("{}") == "YES")
    assert(isBalanced("[]") == "YES")
    assert(isBalanced("[{()}]") == "YES")
    assert(isBalanced("[{(}]") == "NO")
  }
}

