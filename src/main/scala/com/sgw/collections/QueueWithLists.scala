package com.sgw.collections

import scala.annotation.tailrec

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
  * Implement an immutable integer queue using two lists.
  *
  * From: https://www.hackerrank.com/challenges/ctci-queue-using-two-stacks/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=stacks-queues&h_r=next-challenge&h_v=zen
  */
object QueueWithLists {
  case class Queue(private val enq: List[Long] = List(), private val deq: List[Long] = List()) {
    final def enqueue(value: Long): (Long, Queue) = {
      (value, copy(enq = value :: enq))
    }

    @tailrec
    final def dequeue: (Long, Queue) = {
      if (deq.isEmpty) {
        if (enq.isEmpty) {
          throw new RuntimeException("Empty queue")
        } else {
          copy(enq = List(), deq = enq.reverse).dequeue
        }
      } else {
        (deq.head, copy(deq = deq.tail))
      }
    }

    @tailrec
    final def peek: (Long, Queue) = {
      if (deq.isEmpty) {
        if (enq.isEmpty) {
          throw new RuntimeException("Empty queue")
        } else {
          copy(enq = List(), deq = enq.reverse).peek
        }
      } else {
        (deq.head, this)
      }
    }
  }

  def main(args: Array[String]) {

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
    }.foldLeft((List[Long](), Queue())) { case ((acc, queue), (cmd, maybeValue)) =>
      cmd match {
        case 1 =>
          val (_, newQueue) = maybeValue.map(value => queue.enqueue(value)).getOrElse {
            throw new RuntimeException("Command 1 expects a value")
          }
          (acc, newQueue)
        case 2 =>
          val (_, newQueue) = queue.dequeue
          (acc, newQueue)
        case 3 =>
          val (value, newQueue) = queue.peek
          (value :: acc, newQueue)
        case _ =>
          throw new RuntimeException(s"Invalid command $cmd.")
      }
    }._1.reverse

    println(results)

    assert(results == List(14, 14, 60, 78))
  }
}


