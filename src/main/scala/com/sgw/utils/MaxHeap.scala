package com.sgw.utils

import scala.math.Ordering

/**
 * Created by wamplers on 3/14/16.
 */
case class MaxHeap[T](a: Array[T])(implicit ord: Ordering[T]) {
  private var begin = 0

  Heapsort.heapify(a, begin)

  def length: Int = a.length - begin

  def isEmpty: Boolean = length <= 0

  def nonEmpty: Boolean = length > 0

  def head: T = a(begin)

  def headOption: Option[T] = if (nonEmpty) Some(a(begin)) else None

  def dropOne: MaxHeap[T] = if (nonEmpty) {
    begin = begin + 1
    Heapsort.heapify(a, begin)
    this
  } else {
    this
  }

  def drop(n: Int): MaxHeap[T] = {
    (0 until n.min(length)).foreach(_ => dropOne)
    this
  }

  def dropWhile(p: T => Boolean): MaxHeap[T] = {
    while (headOption.exists(p)) {
      dropOne
    }

    this
  }

  def takeAndDropWhile(p: T => Boolean): List[T] = {
    var list = List[T]()

    while (headOption.exists(p)) {
      list = head :: list
      dropOne
    }

    list.reverse
  }

  def takeAndDropWhile(n: Int, dropPredicate: T => Boolean, takePredicate: T => Boolean): List[T] = {
    var list = List[T]()

    while (list.length < n && (headOption.exists(dropPredicate) || headOption.exists(takePredicate))) {
      if (!headOption.exists(dropPredicate) && headOption.exists(takePredicate)) {
        list = head :: list
      }

      dropOne
    }

    list.reverse
  }

  def filter(p: T => Boolean): List[T] = {
    var list = List[T]()

    while (headOption.isDefined) {
      if (p(head)) {
        list = head :: list
      }

      dropOne
    }

    list.reverse
  }

  def toList: List[T] = {
    var list = List[T]()

    while (headOption.isDefined) {
      list = head :: list
      dropOne
    }

    list.reverse
  }
}
