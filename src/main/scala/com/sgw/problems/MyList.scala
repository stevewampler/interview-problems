package com.sgw.problems

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

  def apply(i: Int): T

  def foldLeft[B](z: B)(f: (B, T) => B): B

  def reverse: MyList[T] = foldLeft[MyList[T]](MyNil) {
    case (acc, x) => MyCons(x, acc)
  }

  def ::[E >: T](elem: E): MyList[E] = MyCons(elem, this)
}

object MyNil extends MyList[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("empty list")
  def tail: Nothing = throw new NoSuchElementException("empty list")

  def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z

  def apply(i: Int): Nothing = throw new IndexOutOfBoundsException
}

case class MyCons[+T](head: T, tail: MyList[T] = MyNil) extends MyList[T] {
  def isEmpty: Boolean = false

  def foldLeft[B](z: B)(f: (B, T) => B): B = tail.foldLeft(f(z, head))(f)

  def apply(i: Int): T = if (i == 0) head else tail.apply(i - 1)
}


