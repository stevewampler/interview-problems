package com.sgw.problems

import math.Ordering
import scala.annotation.tailrec

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

  def map[B](f: T => B): MyList[B]

  def filter(p: T => Boolean): MyList[T]

  def filterNot(p: T => Boolean): MyList[T]

  def partition(p: T => Boolean): (MyList[T], MyList[T])

  def takeWhile(p: T => Boolean): MyList[T]

  def dropWhile(p: T => Boolean): MyList[T]

  def span(p: T => Boolean): (MyList[T], MyList[T])

  def pack: MyList[MyList[T]]

  def encode: MyList[(T, Int)] = pack.map(list => (list.head, list.length))

  def foldLeft[B](z: B)(f: (B, T) => B): B

  def foldRight[B](z: B)(f: (T, B) => B): B

  def reverse: MyList[T] = foldLeft[MyList[T]](MyNil) {
    case (acc, x) => MyCons(x, acc)
  }

  def ::[E >: T](elem: E): MyList[E] = MyCons(elem, this)

  def :::[E >: T](list: MyList[E]): MyList[E] = concat(list)

  def splitAt(n: Int): (MyList[T], MyList[T])

  def insertionSort[E >: T](implicit ord: Ordering[E]): MyList[E]

  def insertSorted[E >: T](x: E)(implicit ord: Ordering[E]): MyList[E]

  def mergeSort[E >: T](implicit ord: Ordering[E]): MyList[E]
}

object MyNil extends MyList[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("empty list")
  def tail: MyList[Nothing] = throw new NoSuchElementException("empty list")
  def last: Nothing = throw new NoSuchElementException("empty list")
  def init: MyList[Nothing] = throw new NoSuchElementException("empty list")

  def concat[B >: Nothing](ys: MyList[B]): MyList[B] = ys

  def take(n: Int): MyList[Nothing] = MyNil
  def drop(n: Int): MyList[Nothing] = MyNil

  def length: Int = 0

  def removeAt(n: Int): MyList[Nothing] = MyNil

  def flatten: MyList[Nothing] = MyNil

  def map[B](f: Nothing => B): MyList[B] = MyNil

  def filter(p: Nothing => Boolean): MyList[Nothing] = MyNil

  def filterNot(p: Nothing => Boolean): MyList[Nothing] = MyNil

  def partition(p: Nothing => Boolean): (MyList[Nothing], MyList[Nothing]) = (MyNil, MyNil)

  def takeWhile(p: Nothing => Boolean): MyList[Nothing] = MyNil

  def dropWhile(p: Nothing => Boolean): MyList[Nothing] = MyNil

  def span(p: Nothing => Boolean): (MyList[Nothing], MyList[Nothing]) = (MyNil, MyNil)

  def pack: MyList[MyList[Nothing]] = MyNil

  def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z

  def foldRight[B](z: B)(f: (Nothing, B) => B): B = z

  def splitAt(n: Int): (MyList[Nothing], MyList[Nothing]) = (MyNil, MyNil)

  def apply(i: Int): Nothing = throw new IndexOutOfBoundsException

  def insertionSort[E >: Nothing](implicit ord: Ordering[E]): MyList[E] = MyNil

  def insertSorted[E >: Nothing](x: E)(implicit ord: Ordering[E]): MyList[E] = x :: MyNil

  def mergeSort[E >: Nothing](implicit ord: Ordering[E]): MyList[E] = MyNil

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

  def length: Int = // 1 + tail.length
    foldRight(0) {
      case (x, acc) => acc + 1
    }

  def map[B](f: T => B): MyList[B] = // f(head) :: tail.map(f)
    foldRight(MyList[B]()) {
      case (x, acc) => f(x) :: acc
    }

  def filter(p: T => Boolean): MyList[T] = if (p(head)) {
    head :: tail.filter(p)
  } else {
    tail.filter(p)
  }

  def filterNot(p: T => Boolean): MyList[T] = if (p(head)) {
    tail.filterNot(p)
  } else {
    head :: tail.filterNot(p)
  }

  def partition(p: T => Boolean): (MyList[T], MyList[T]) = (filter(p), filterNot(p))
//    foldLeft((MyList[T](), MyList[T]())) {
//      case ((left, right), x) => if (p(x)) {
//        (x :: left, right)
//      } else {
//        (left, x :: right)
//      }
//    }

  def takeWhile(p: T => Boolean): MyList[T] =
    if (p(head)) {
      head :: tail.takeWhile(p)
    } else {
      MyNil
    }

  def dropWhile(p: T => Boolean): MyList[T] =
    if (p(head)) {
      tail.dropWhile(p)
    } else {
      this
    }

  def span(p: T => Boolean): (MyList[T], MyList[T]) = (takeWhile(p), dropWhile(p))

  def pack: MyList[MyList[T]] =
    takeWhile(x => x == head) :: dropWhile(x => x == head).pack

  def foldLeft[B](z: B)(f: (B, T) => B): B = tail.foldLeft(f(z, head))(f)

  def foldRight[B](z: B)(f: (T, B) => B): B = f(head, tail.foldRight(z)(f))

  def apply(i: Int): T = if (i == 0) head else tail.apply(i - 1)

  // complexity is N * N because, in the worst case, we have to visit all N values and then insert all N values
  def insertionSort[E >: T](implicit ord: Ordering[E]): MyList[E] = tail.insertionSort(ord).insertSorted(head)

  def splitAt(n: Int): (MyList[T], MyList[T]) = (take(n), drop(n))

  def insertSorted[E >: T](x: E)(implicit ord: Ordering[E]): MyList[E] = if (x == head || ord.lt(x, head)) {
    x :: this
  } else {
    head :: tail.insertSorted(x)
  }

  def mergeSort[E >: T](implicit ord: Ordering[E]): MyList[E] = {
    val n = length / 2

    if (n == 0)
      this
    else {
      def merge(left: MyList[E], right: MyList[E]): MyList[E] = (left, right) match {
        case (_, MyNil) => left
        case (MyNil, _) => right
        case (MyCons(leftHead, leftTail), MyCons(rightHead, rightTail)) => if (ord.lt(leftHead, rightHead)) {
          leftHead :: merge(leftTail, right)
        } else {
          rightHead :: merge(left, rightTail)
        }
      }

      val (left, right) = splitAt(n)

      merge(left.mergeSort(ord), right.mergeSort(ord))
    }
  }

  override def toString = s"$head${if (tail != MyNil) "," else ""}$tail"
}


