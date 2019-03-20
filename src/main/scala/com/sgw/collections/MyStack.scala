package com.sgw.collections

case class MyStack[T](private val list: List[T] = List.empty[T]) {
  def size: Int = list.size

  def isEmpty: Boolean = size == 0
  def nonEmpty: Boolean = !isEmpty

  def push(value: T): MyStack[T] = MyStack(value :: list)

  def pop: (T, MyStack[T]) = (list.head, MyStack(list.tail))
}

object MyStack {
  def main(args: Array[String]): Unit = {
    val stack = MyStack[Int]()
    assert(stack.isEmpty)
    assert(!stack.nonEmpty)

    val stack2 = stack.push(1)
    assert(stack2.size == 1)
    assert(stack2.nonEmpty)

    val (v, stack3) = stack2.pop

    assert(v == 1)
    assert(stack3.isEmpty)
    assert(stack3.size == 0)
  }
}
