package com.sgw.coursera

import scala.annotation.tailrec

/**
  * Write a recursive function which verifies the balancing of parentheses in a string, which we
  * represent as a List[Char] not a String. For example, the function should return true for the following strings:
  *
  * (if (zero? x) max (/ 1 x))
  * I told him (that it's not (yet) done).\n(But he wasn't listening)
  *
  * The function should return false for the following strings:
  *
  * :-)
  * ())(
  *
  * The last example shows that it’s not enough to verify that a string contains the same number of
  * opening and closing parentheses.
  */
object BalanceParens extends App { // TODO objects are where you put static functions, members, ... an object that extends App application
  /**
    * Returns true if the parenthesis in the specified list of characters is balanced.
    */
  def balance(chars: List[Char]): Boolean = { // functions always take the form "def f(x) = y" ... the "=" is important
    @tailrec // TODO scala supports tail-call optimization if the function is tail recursive @tailrec will fail if the function is not tail resursive
    def go(chars: List[Char], count: Int = 0): Int = { // TODO functions can be defined within functions
      if (chars.isEmpty || count < 0) { // TODO if statements are functions
        count
      } else {
        val headChar: Char = chars.head // TODO get the head (first element) of the list of characters
        val tailChars: List[Char] = chars.tail // TODO get the tail (remaining elements) of the list of characters

        val newCount: Int = headChar match { // TODO pattern matching on the head character
          case '(' => count + 1
          case ')' => count - 1
          case _ => count // TODO the '_' character here means "everything else"
        }

        // TODO recurse to process the tail characters
        go(tailChars, newCount) // TODO the value of the last expression in a function is the value the function returns (no need for a "return" statement
      }
    }

    go(chars) == 0 // TODO: if the result of the recursion is zero, then the parens are balanced
  }

  /**
    * Returns true if the parenthesis in the specified string is balanced.
    */
  // TODO this is just a function that calls another function after converting the string to a list of characters
  def balance(str: String): Boolean = balance(str.toList) // TODO no need for "{ ... }"

  /**
    * Returns true if the parenthesis in the specified list of characters is balanced.
    */
  // TODO another implementation of the balance function that uses "foldLeft" instead of recursion
  def balance2(chars: List[Char]): Boolean = { // TODO functions always take the form "def f(x) = y" ... the "=" is important
    val finalCount: Int = chars.foldLeft(0) { // TODO here we're "folding" an integer (count) over all of the characters and modifying the count iff the a character is a paren
      case (count, '(') if count >= 0 => count + 1 // TODO each of these "case" statements define a "partial function". E.g. this one only applies if the character is a '(' and the count is >= 0
      case (count, ')') if count >= 0 => count - 1
      case (count, _) => count // TODO not a paren, so don't change the count
    }

    finalCount == 0
  }

  /**
    * Returns true if the parenthesis in the specified string is balanced.
    */
  def balance2(str: String): Boolean = balance2(str.toList)

    // TODO the body of the App-derived "object" is effectively application's "main" method
  // TODO with an "App" derived "object' the "args: Array[String]" variable is already in scope
  args.foreach { str =>
    println(s"$str = ${balance(str)}") // TODO string interpolation calling
  }

  // TODO You can optionally define your own a "def main(args: Array[String]): Int" function and don't derive from "App"

  // TODO "val" means the value can't change. "var" means it can.
  val list: List[(String, Boolean)] = List( // TODO you can declare a list (or any other scala collection) inline like this ...
    // TODO three ways to specify tuples:
    "(( ) )" -> true, // using "->"
    ("(( ) )", true), // using "(x, y)"
    Tuple2("(( ) )", true), // using Tuple2(x, y)

    "(" -> false,
    "((() )" -> false,
    ")(" -> false,
    "())(" -> false,
    "(if (zero? x) max (/ 1 x))" -> true,
    "I told him (that it's not (yet) done).\\n(But he wasn't listening)" -> true,
    ":-)" -> false,
    "))((" -> false
  )

  val foo = list.map { case (str, expected) => // TODO here we're mapping over the list of strings to check to see which ones are balanced
    (str, expected, balance(str)) // TODO creating a three tuple of the string, the expected answer, and the actual answer
  }.map { case (str, expected, actual) => // TODO "case" here is
    println(s"expected=$expected actual=$actual str=$str") // TODO string interpolation
    assert(actual == expected)
    actual
  }

  foo

  println("-----")

  list.map { case (str, expected) => // TODO here we're mapping over the list of strings to check to see which ones are balanced
    (str, expected, balance2(str)) // TODO creating a three tuple of the string, the expected answer, and the actual answer
  }.foreach { case (str, expected, actual) => // TODO "case" here is
    println(s"expected=$expected actual=$actual str=$str") // TODO string interpolation
    assert(actual == expected)
  }
}
