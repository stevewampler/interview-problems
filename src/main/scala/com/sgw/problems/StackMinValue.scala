package com.sgw.problems

/**
  * A stack's push and pop operations are O(1). Add a min operation that is also O(1).
  *
  * A solution is to keep a 2nd stack whose head contains the current minimum value on the primary stack.
  */
object StackMinValue extends App {
  class Stack {
    private var stack = List[Int]()
    private var minStack = List[Int]() // the top of this stack should always contain the min value

    // O(1)
    def push(i: Int): Unit = {
      stack = i :: stack

      // push the value onto the min-stack iff the min-stack is empty or it contains a value >= the pushed value
      if (minStack.isEmpty || i <= minStack.head) {
        minStack = i :: minStack
      }
    }

    // O(1)
    def pop(): Int = {
      val head = stack.head
      stack = stack.tail

      // pop the value off of the min-stack iff it's the same as the top of the min-stack
      if (head == minStack.head) {
        minStack = minStack.tail
      }

      head
    }

    // O(1)
    def min(): Int = minStack.head // the top of the min-stack is always the minimum value
  }

  val stack = new Stack

  stack.push(3)
  assert(stack.min() == 3)
  stack.push(5)
  assert(stack.min() == 3)
  stack.push(-1)
  assert(stack.min() == -1)
  stack.push(5)
  assert(stack.min() == -1)
  stack.push(-1)
  assert(stack.min() == -1)

  assert(stack.pop() == -1)
  assert(stack.min() == -1)
  assert(stack.pop() == 5)
  assert(stack.min() == -1)
  assert(stack.pop() == -1)
  assert(stack.min() == 3)
  assert(stack.pop() == 5)
  assert(stack.min() == 3)
  assert(stack.pop() == 3)
}
