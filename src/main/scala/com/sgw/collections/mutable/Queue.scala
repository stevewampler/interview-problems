package com.sgw.collections.mutable

import scala.reflect.ClassTag

// mutable
case class Queue[T : ClassTag](capacity: Int = 10) {
  private var _size: Int = 0
  private val arr = Array.ofDim[T](capacity)

  private var head: Int = 0 // this is where the next item will be dequeued
  private var next: Int = 0 // this is where the next item will be enqueued

  def size: Int = _size

  def isEmpty: Boolean = size == 0
  def nonEmpty: Boolean = !isEmpty

  def enqueue(value: T): Queue[T] = {
    if (next >= arr.length) {
      next = 0
    }

    if (_size == capacity) {
      throw new RuntimeException("Queue is full.")
    }

    arr.update(next, value)

    next = next + 1

    _size = _size + 1

    this
  }

  def dequeue: T = {
    if (_size == 0) {
      throw new NoSuchElementException("The queue is empty.")
    }

    if (head >= arr.length) {
      head = 0
    }

    val value = arr(head)

    head = head + 1

    _size = _size - 1

    value
  }
}

object Queue {
  def main(args: Array[String]): Unit = {
    val queue = Queue[Int](5)

    assert(queue.size == 0)
    assert(queue.isEmpty)
    assert(!queue.nonEmpty)

    queue.enqueue(1)
    assert(queue.size == 1)
    assert(!queue.isEmpty)
    assert(queue.nonEmpty)

    queue.enqueue(2)
    queue.enqueue(3)
    assert(queue.size == 3)
    assert(!queue.isEmpty)
    assert(queue.nonEmpty)

    assert(queue.dequeue == 1)
    assert(queue.size == 2)
    assert(!queue.isEmpty)
    assert(queue.nonEmpty)

    assert(queue.dequeue == 2)
    assert(queue.size == 1)
    assert(!queue.isEmpty)
    assert(queue.nonEmpty)

    assert(queue.dequeue == 3)
    assert(queue.size == 0)
    assert(queue.isEmpty)
    assert(!queue.nonEmpty)

    queue.enqueue(4)
    queue.enqueue(5)
    queue.enqueue(6)
    assert(queue.size == 3)
    assert(!queue.isEmpty)
    assert(queue.nonEmpty)
    assert(queue.dequeue == 4)
    assert(queue.size == 2)
    assert(!queue.isEmpty)
    assert(queue.nonEmpty)

    assert(queue.dequeue == 5)
    assert(queue.size == 1)
    assert(!queue.isEmpty)
    assert(queue.nonEmpty)

    assert(queue.dequeue == 6)
    assert(queue.size == 0)
    assert(queue.isEmpty)
    assert(!queue.nonEmpty)
  }
}
