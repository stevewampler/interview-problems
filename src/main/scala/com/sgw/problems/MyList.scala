package com.sgw.problems

import math.Ordering

object MyList {
  def apply[T](): MyList[T] = MyNil

  def apply[T](head: T, tail: MyList[T]): MyList[T] = MyCons[T](head, tail)

  def apply[T](list: T*): MyList[T] = {
    var acc: MyList[T] = MyNil

    list.foreach { i =>
      acc = MyCons(i, acc)
    }

    acc.reverse
  }
}

trait MyList[+T] {
  def isEmpty: Boolean

  def head: T
  def tail: MyList[T]
  def last: T
  def init: MyList[T]

  def take(n: Int): MyList[T]
  def drop(n: Int): MyList[T]

  def concat[E >: T](list: MyList[E]): MyList[E]

  def flatten: MyList[Any]

  def removeAt(n: Int): MyList[T]

  def length: Int

  def apply(i: Int): T

  def foldLeft[B](z: B)(f: (B, T) => B): B

  def reverse: MyList[T] = foldLeft[MyList[T]](MyNil) {
    case (acc, x) => MyCons(x, acc)
  }

  def ::[E >: T](elem: E): MyList[E] = MyCons(elem, this)

  def :::[E >: T](list: MyList[E]): MyList[E] = concat(list)

  def splitAt(n: Int): (MyList[T], MyList[T])

  def insertionSort[E >: T](implicit lt: (E, T) => Boolean): MyList[E]

  def insertSorted[E >: T](x: E)(implicit lt: (E, T) => Boolean): MyList[E]

  def mergeSort(lt: (T, T) => Boolean): MyList[T]
}

object MyNil extends MyList[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("empty list")
  def tail: MyList[Nothing] = throw new NoSuchElementException("empty list")
  def last: Nothing = throw new NoSuchElementException("empty list")
  def init: MyList[Nothing] = throw new NoSuchElementException("empty list")

  def concat[B >: Nothing](ys: MyList[B]): MyList[B] = ys

  def take(n: Int): MyList[Nothing] = this
  def drop(n: Int): MyList[Nothing] = this

  def length: Int = 0

  def removeAt(n: Int): MyList[Nothing] = MyNil

  def flatten: MyList[Nothing] = this

  def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z

  def splitAt(n: Int): (MyList[Nothing], MyList[Nothing]) = (MyNil, MyNil)

  def apply(i: Int): Nothing = throw new IndexOutOfBoundsException

  def insertionSort[E >: Nothing](implicit lt: (E, Nothing) => Boolean): MyList[E] = MyNil

  def insertSorted[E >: Nothing](x: E)(implicit lt: (E, Nothing) => Boolean): MyList[E] = x :: MyNil

  def mergeSort(lt: (Nothing, Nothing) => Boolean): MyList[Nothing] = MyNil

  override def toString = ""
}

case class MyCons[+T](head: T, tail: MyList[T] = MyNil) extends MyList[T] {
  def isEmpty: Boolean = false

  def last: T = tail match {
    case MyNil => head
    case _ => tail.last
  }

  def init: MyList[T] = tail match {
    case MyNil => MyNil
    case _ => MyCons(head, tail.init)
  }

  def concat[B >: T](ys: MyList[B]): MyList[B] = ys match {
    case MyNil => this
    case _ => MyCons(ys.head, concat(ys.tail))
  }

  def flatten: MyList[Any] = head match {
    case xs: MyList[_] => tail.flatten.concat(xs.flatten)
    case _ => MyCons(head, tail.flatten)
  }

  def take(n: Int): MyList[T] =
    if (n <= 0) MyNil
    else MyCons(head, tail.take(n - 1))

  def drop(n: Int): MyList[T] =
    if (n <= 0) this
    else tail.drop(n - 1)

  def removeAt(n: Int): MyList[T] =
    if (n < 0)
      this
    else if (n == 0)
      tail
    else
      MyCons(head, tail.removeAt(n - 1))

  def length: Int = 1 + tail.length

  def foldLeft[B](z: B)(f: (B, T) => B): B = tail.foldLeft(f(z, head))(f)

  def apply(i: Int): T = if (i == 0) head else tail.apply(i - 1)

  // complexity is N * N because, in the worst case, we have to visit all N values and then insert all N values
  def insertionSort[E >: T](implicit lt: (E, T) => Boolean): MyList[E] = tail.insertionSort.insertSorted(head)

  def splitAt(n: Int): (MyList[T], MyList[T]) = (take(n), drop(n))

  def insertSorted[E >: T](x: E)(implicit lt: (E, T) => Boolean): MyList[E] = if (x == head || lt(x, head)) {
    x :: this
  } else {
    head :: tail.insertSorted(x)
  }

  def mergeSort(lt: (T, T) => Boolean): MyList[T] = {
    val n = length / 2

    if (n == 0)
      this
    else {
      def merge(left: MyList[T], right: MyList[T]): MyList[T] = (left, right) match {
        case (_, MyNil) => left
        case (MyNil, _) => right
        case (MyCons(leftHead, leftTail), MyCons(rightHead, rightTail)) => if (lt(leftHead, rightHead)) {
          leftHead :: merge(leftTail, right)
        } else {
          rightHead :: merge(left, rightTail)
        }
      }

      val (left, right) = splitAt(n)

      merge(left.mergeSort(lt), right.mergeSort(lt))
    }
  }

  override def toString = s"$head${if (tail != MyNil) "," else ""}$tail"
}


