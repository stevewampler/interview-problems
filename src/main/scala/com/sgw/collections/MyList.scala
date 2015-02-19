package com.sgw.collections

import scala.annotation.tailrec

trait MyList[+T] {
  def head: T
  def tail: MyList[T]

  def append[U >: T](value: U): MyList[U] = Cons(head, tail.append(value))

  // O(n^2) and not tail recursive
  def reverse: MyList[T] = tail.reverse.append(head)

  @tailrec
  final def foldLeft[B](z: B)(f: (B, T) => B): B = this match {
    case Cons(h, t) => t.foldLeft(f(z, h))(f)
    case Nil => z
  }

  // O(n), tail recursive, uses foldLeft
  def reverse2: MyList[T] = foldLeft(Nil: MyList[T])((t, h) => Cons(h, t))
}

case class Cons[T](head: T, tail: MyList[T]) extends MyList[T]

object Nil extends MyList[Nothing] {
  def head = throw new RuntimeException("no head")
  def tail = throw new RuntimeException("no tail")

  override def append[U >: Nothing](value: U): MyList[U] = Cons(value, Nil)

  override def reverse: MyList[Nothing] = Nil

  override def toString: String = "Nil"
}

object TestReverse {
  def main(args: Array[String]): Unit = {
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    println(list)
    println(list.reverse)
    println(list.reverse2)
  }
}



