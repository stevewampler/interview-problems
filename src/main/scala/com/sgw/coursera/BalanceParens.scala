package com.sgw.coursera

import scala.annotation.tailrec

/**
  * Write a recursive function which verifies the balancing of parentheses in a string, which we
  * represent as a List[Char] not a String. For example, the function should return true for the following strings:
  *
  * (if (zero? x) max (/ 1 x))
  * I told him (that it’s not (yet) done). (But he wasn’t listening)
  *
  * The function should return false for the following strings:
  *
  * :-)
  * ())(
  *
  * The last example shows that it’s not enough to verify that a string contains the same number of
  * opening and closing parentheses.
  */
object BalanceParens {
  /**
    * Returns true if the parenthesis in the specified list of characters is balanced.
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def go(chars: List[Char], count: Int = 0): Int = {
      if (chars.isEmpty || count < 0) return count

      go(chars.tail, chars.head match {
        case '(' => count + 1
        case ')' => count - 1
        case _ => count
      })
    }

    go(chars) == 0
  }
}
