package com.sgw.collections.mutable

import com.sgw.collections.immutable

/**
  * A queue is an abstract data type that maintains the order in which elements were added to it, allowing the oldest
  * elements to be removed from the front and new elements to be added to the rear. This is called a First-In-First-Out (FIFO)
  * data structure because the first element added to the queue (i.e., the one that has been waiting the longest) is
  * always the first one to be removed.
  *
  * A basic queue has the following operations:
  *
  * Enqueue: add a new element to the end of the queue.
  * Dequeue: remove the element from the front of the queue and return it.
  * In this challenge, you must first implement a queue using two stacks. Then process  queries, where each query
  * is one of the following types:
  *
  * 1 x: Enqueue element x into the end of the queue.
  * 2: Dequeue the element at the front of the queue.
  * 3: Print the element at the front of the queue.
  *
  * Implement an integer queue using two stacks.
  *
  * From: https://www.hackerrank.com/challenges/ctci-queue-using-two-stacks/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=stacks-queues&h_r=next-challenge&h_v=zen
  */
object QueueWithStacks {
  private def move(fromStack: immutable.Stack[Long], toStack: immutable.Stack[Long]) = {
    var fromStackTmp = fromStack
    var toStackTmp   = toStack

    while (fromStackTmp.nonEmpty) {
      val (value, newFromStack) = fromStack.pop
      val newToStack = toStack.push(value)
      fromStackTmp = newFromStack
      toStackTmp   = newToStack
    }
  }

  case class Queue() {
    private var enqueueStack = immutable.Stack[Long]()
    private var dequeueStack = immutable.Stack[Long]()

    def enqueue(value: Long): Long = {
      enqueueStack = enqueueStack.push(value)
      value
    }

    def dequeue: Long = {
      if (dequeueStack.isEmpty) {
        move(enqueueStack, dequeueStack)
      }

      val (value, newDequeueStack) = dequeueStack.pop

      dequeueStack = newDequeueStack

      value
    }

    def peek: Long = {
      if (dequeueStack.isEmpty) {
        move(enqueueStack, dequeueStack)
      }

      dequeueStack.top
    }
  }

  def main(args: Array[String]) {

    val queue = Queue()

    val cmds = List(
      "1 42",
      "2",
      "1 14",
      "3",
      "1 28",
      "3",
      "1 60",
      "1 78",
      "2",
      "2",
      "3",
      "2",
      "3"
    )

    val results = cmds.map(_.split(' ')).map {
      case Array(cmd) => (cmd.toInt, None)
      case Array(cmd, value) => (cmd.toInt, Some(value.toLong))
    }.foldLeft(List[Long]()) { case (acc, (cmd, maybeValue)) =>
      cmd match {
        case 1 => maybeValue.map(queue.enqueue).getOrElse {
          throw new RuntimeException("Command 1 expects a value")
        }; acc
        case 2 => queue.dequeue; acc
        case 3 => queue.peek :: acc
        case _ => throw new RuntimeException(s"Invalid command $cmd.")
      }
    }.reverse

    println(results)

    assert(results == List(14, 14, 60, 78))
  }
}
