package com.sgw.windowed

import scala.annotation.tailrec

object Queue {
  def empty[T] = Queue[T]()

  def apply[T](xs: T*): Queue[T] = new Queue[T](
    deq = xs.toList
  )
}

case class Queue[T](
  private val enq: List[T] = List.empty[T],
  private val deq: List[T] = List.empty[T]
) {

  final def enqueue(value: T): Queue[T] = {
    copy(enq = value :: enq)
  }

  @tailrec
  final def dequeue: (T, Queue[T]) = {
    if (deq.isEmpty) {
      if (enq.isEmpty) {
        throw new RuntimeException("Empty queue")
      } else {
        copy(enq = List.empty[T], deq = enq.reverse).dequeue
      }
    } else {
      (deq.head, copy(deq = deq.tail))
    }
  }

  def dequeueWhile(p: T => Boolean): (List[T], Queue[T]) = {
    @tailrec
    def go(queue: Queue[T], list: List[T] = List.empty[T]): (List[T], Queue[T]) = {
      if (queue.isEmpty) return (list, queue)

      val (v1, newQueue1) = queue.peek

      // if the value passes the predicate ...
      if (p(v1)) {
        // dequeue the value
        val (v2, newQueue2) = newQueue1.dequeue

        // keep going
        go(newQueue2, v2 :: list)
      } else {
        // we're done
        (list, newQueue1)
      }
    }

    go(queue = this)
  }

  @tailrec
  final def peek: (T, Queue[T]) = {
    if (deq.isEmpty) {
      if (enq.isEmpty) {
        throw new RuntimeException("Empty queue")
      } else {
        copy(enq = List.empty[T], deq = enq.reverse).peek
      }
    } else {
      (deq.head, this)
    }
  }

  final def maybePeek: (Option[T], Queue[T]) = {
    if (nonEmpty) {
      val (value, newQueue) = peek

      (Some(value), newQueue)
    } else {
      (None, this)
    }
  }

  final def size: Int = enq.size + deq.size

  final def isEmpty: Boolean = size == 0

  final def nonEmpty: Boolean = !isEmpty
}
