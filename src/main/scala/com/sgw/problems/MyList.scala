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

  def sort(lt: (T, T) => Boolean): MyList[T]

  def insertSorted[E >: T](x: E, lt: (E, T) => Boolean): MyList[E]
}

object MyNil extends MyList[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("empty list")
  def tail: Nothing = throw new NoSuchElementException("empty list")

  def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z

  def apply(i: Int): Nothing = throw new IndexOutOfBoundsException

  def sort(lt: (Nothing, Nothing) => Boolean): MyList[Nothing] = MyNil

  def insertSorted[E >: Nothing](x: E, lt: (E, Nothing) => Boolean): MyList[E] = x :: MyNil

  override def toString = ""
}

case class MyCons[+T](head: T, tail: MyList[T] = MyNil) extends MyList[T] {
  def isEmpty: Boolean = false

  def foldLeft[B](z: B)(f: (B, T) => B): B = tail.foldLeft(f(z, head))(f)

  def apply(i: Int): T = if (i == 0) head else tail.apply(i - 1)

  // complexity is N * N because, in the worst case, we have to visit all N values and then insert all N values
  def sort(lt: (T, T) => Boolean): MyList[T] =
    tail.sort(lt).insertSorted(head, lt)

  def insertSorted[E >: T](x: E, lt: (E, T) => Boolean): MyList[E] = if (x == head || lt(x, head)) {
    x :: this
  } else {
    head :: tail.insertSorted(x, lt)
  }

  override def toString = s"$head${if (tail != MyNil) "," else ""}$tail"
}


