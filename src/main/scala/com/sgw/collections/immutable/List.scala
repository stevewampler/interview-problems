package com.sgw.collections.immutable

import scala.annotation.tailrec

object List {
  def empty[A]: List[A] = Nil

  def apply[A](values: A*): List[A] = if (values.isEmpty) {
    Nil
  } else {
    values.head :: apply[A](values.tail:_*)
  }
}

trait List[+T] {
  def head: T
  def tail: List[T]

  def prepend[U >: T](value: U): List[U] = Cons(head, tail.prepend(value))

  def size: Int
  def isEmpty: Boolean

  def ::[U >: T](value: U): List[U] = prepend(value)

  def map[B](f: T => B): List[B] = foldLeft[List[B]](List.empty[B]) { case (list, value) =>
    f(value) :: list
  }.reverse

  // O(n^2) and not tail recursive
  def reverse: List[T] = tail.reverse.prepend(head)

  @tailrec
  final def foldLeft[B](z: B)(f: (B, T) => B): B = this match {
    case Cons(h, t) => t.foldLeft(f(z, h))(f)
    case Nil => z
  }

  // O(n), tail recursive, uses foldLeft
  def reverse2: List[T] = foldLeft(Nil: List[T])((t, h) => Cons(h, t))
}

case class Cons[T](head: T, tail: List[T]) extends List[T] {
  override def size: Int = 1 + tail.size
  override def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  override def head = throw new RuntimeException("no head")
  override def tail = throw new RuntimeException("no tail")

  override def size: Int = 0
  override def isEmpty: Boolean = true

  override def prepend[U >: Nothing](value: U): List[U] = Cons(value, Nil)

  override def reverse: List[Nothing] = Nil

  override def toString: String = "Nil"
}

object TestList {
  def main(args: Array[String]): Unit = {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    println(list)
    println(list.reverse)
    println(list.reverse2)

    val list2 = List(1, 2, 3, 4, 5)

    println(list2)
  }
}



