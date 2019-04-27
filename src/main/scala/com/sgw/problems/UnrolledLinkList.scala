package com.sgw.problems

import com.sgw.problems.UnrolledLinkList.Node

object UnrolledLinkList {
  case class Node(
    var array: Array[Char],
    var size: Int = 0,
    var maybeNext: Option[Node] = None
  ) {
    private def findNode(index: Int): Option[(Node, Int)] = {
      if (index < size) return Some((this, index))

      maybeNext.flatMap { nextNode =>
        nextNode.findNode(index - size)
      }
    }

    private def isFull: Boolean = size >= array.length
    private def notFull: Boolean = !isFull

    private def insertNewNode(value: Char, index: Int): Unit = {
      // create a new node
      val dstNode = Node(array = Array.ofDim[Char](array.length), maybeNext = maybeNext)
      maybeNext = Some(dstNode)

      val dstSize = size - index
      Array.copy(array, index, dstNode.array, 0, dstSize)
      dstNode.size = dstSize

      size = size - dstSize
      array(size) = value
      size = size + 1
    }

    def insertImpl(value: Char, index: Int): Unit =
      if (notFull) {
        if (index < size) {
          insertNewNode(value, index)
        } else {
          array(index) = value
          size = size + 1
        }
      } else {
        insertNewNode(value, index)
      }

    def insert(value: Char, index: Int): Unit =
      findNode(index).map { case (node, index) =>
        node.insertImpl(value, index)
      }.getOrElse {
        throw new RuntimeException(s"Index $index out of bounds.")
      }

    def add(value: Char): Unit = {
      maybeNext.map { next =>
        next.add(value)
      }.getOrElse {
        if (notFull) {
          array(size) = value
          size = size + 1
        } else {
          val next = Node(array = Array.ofDim[Char](array.length))
          next.array(0) = value
          next.size = 1
          maybeNext = Some(next)
        }
      }
    }

    def get(index: Int): Option[Char] =
      findNode(index).map { case (node, index) =>
        node.array(index)
      }

    override def toString: String = s"${array.take(size).mkString(",")}${maybeNext.map { node =>
      s",${node.toString}"
    }.getOrElse("")}"
  }

  def main(args: Array[String]): Unit = {
    val list = UnrolledLinkList(5)

    list.add(value = 'a') // a
    list.add(value = 'b') // a, b
    list.add(value = 'c') // a, b, c
    list.add(value = 'e') // a, b, c, e

    list.insert(value = 'd', index = 3) // a, b, c, d, e

    list.insert(value = 'x', index = 4) // a, b, c, d, x, e
    list.insert(value = 'y', index = 4) // a, b, c, d, y, x, e
    list.insert(value = 'z', index = 5) // a, b, c, d, y, z, x, e

    println(list)

    println(list.get(0))
    println(list.get(2))
    println(list.get(6))
    println(list.get(7))
  }
}

/**
  * Implement the get, insert, and remove methods for an UnrolledLinkedList, which is a link list with arrays
  * attached to each node.
  */
case class UnrolledLinkList(nodeCapacity: Int = 5) {

  private val root: Node = Node(
    array = Array.ofDim[Char](nodeCapacity)
  )

  def insert(value: Char, index: Int): Unit = {
    if (index < 0 || index > size) throw new IndexOutOfBoundsException(s"Index $index is invalid.")

    root.insert(value, index)
  }

  def add(value: Char): Unit = root.add(value)

  def remove(index: Int): UnrolledLinkList = ???

  def get(index: Int): Option[Char] = root.get(index)

//  private def findNode(index: Int): Node = root.findNode(index)

  private def size(maybeNode: Option[Node]): Int = maybeNode.map { node =>
    node.size + size(node.maybeNext)
  }.getOrElse(0)

  def size: Int = size(Some(root))

  override def toString: String = {
    s"UnrolledLinkList(${root.toString})"
  }
}
