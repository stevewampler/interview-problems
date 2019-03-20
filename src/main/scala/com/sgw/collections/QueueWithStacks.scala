package com.sgw.collections


import java.io._
import scala.collection._

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
  private def move(fromStack: mutable.Stack[Long], toStack: mutable.Stack[Long]) = {
    while (fromStack.nonEmpty) {
      toStack.push(fromStack.pop)
    }
  }

  case class Queue() {
    private val enqueueStack = mutable.Stack[Long]()
    private val dequeueStack = mutable.Stack[Long]()

    def enqueue(value: Long): Long = {
      enqueueStack.push(value)
      value
    }

    def dequeue: Long = {
      if (dequeueStack.isEmpty) {
        move(enqueueStack, dequeueStack)
      }

      dequeueStack.pop
    }

    def peek: Long = {
      if (dequeueStack.isEmpty) {
        move(enqueueStack, dequeueStack)
      }

      dequeueStack.top
    }
  }

//  case class Queue3() {
//    private var head: Int = 0
//    private var next: Int = 0
//    private var size: Int = 0
//    private var arr = Array.ofDim[Long](100)
//
//    private def checkEmpty = {
//      if (isEmpty) {
//        throw new RuntimeException("Queue is empty.")
//      }
//    }
//
//    private def isFull = size == arr.length
//
//    override def toString: String = s"head=$head, next=$next, size=$size, arr=${arr.mkString(",")}"
//
//    private def growArray = {
//      val newArr = Array.ofDim[Long](arr.size * 2)
//
//      // copy from head to the end of the array
//      Array.copy(arr, head, newArr, 0, arr.size - head)
//
//      // copy from the beginning of the array to the head
//      Array.copy(arr, 0, newArr, arr.size - head, head)
//
//      head = 0
//      next = arr.size
//      arr = newArr
//    }
//
//    def isEmpty: Boolean = size == 0
//
//    def nonEmpty: Boolean = !isEmpty
//
//    def enqueue(value: Long): Long = {
//      arr(next) = value
//
//      next = next + 1
//
//      size = size + 1
//
//      if (isFull) {
//        growArray
//      } else if (next == arr.size) {
//        next = 0
//      }
//
//      value
//    }
//
//    def dequeue: Long = {
//      checkEmpty
//
//      val value = arr(head)
//
//      head = head + 1
//
//      if (head == arr.size) {
//        head = 0
//      }
//
//      size = size - 1
//
//      value
//    }
//
//    def peek: Long = {
//      checkEmpty
//      arr(head)
//    }
//  }

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


