package com.sgw.collections.immutable

import scala.annotation.tailrec

object Tree {
  @tailrec
  def breadthFirstSearchImpl[U](x: U, queue: Queue[INode[U]]): Option[INode[U]] = if (queue.isEmpty) {
    None
  } else {
    queue.dequeue match {
      case (head, tail) => head match {
        case Node(v, _, _) if v == x => Some(head)
        case Node(_, l, r) => {
          breadthFirstSearchImpl(x, tail.enqueue(l).enqueue(r))
        }
        case _ => breadthFirstSearchImpl(x, tail)
      }
    }
  }
}

trait INode[+T] {
  def value: T
  def left:  INode[T]
  def right: INode[T]

//  def breadthFirstSearch[U >: T](x: U): Option[INode[U]] = Tree.breadthFirstSearchImpl(x, Queue[INode[U]]().enqueue(value = this))
//
//  def depthFirstSearch[U >: T](x: U): Option[INode[U]] = this match {
//    case Node(v, _, _) if x == v => Some(this)
//    case Node(_, l, r) => l.depthFirstSearch(x) orElse r.depthFirstSearch(x)
//    case NilNode => None
//  }

  def breadthFirstSearch[U >: T](x: U): Option[INode[U]] = Tree.breadthFirstSearchImpl(x, Queue.empty[INode[U]].enqueue(this))

  def depthFirstSearch[U >: T](x: U): Option[INode[U]] = this match {
    case Node(v, _, _) if x == v => Some(this)
    case Node(_, l, r) => l.depthFirstSearch(x) orElse r.depthFirstSearch(x)
    case NilNode => None
  }
}

case class Node[T](value: T, left: INode[T] = NilNode, right: INode[T] = NilNode) extends INode[T]

object NilNode extends INode[Nothing] {
  def value = throw new RuntimeException("no value")
  def left  = throw new RuntimeException("no left")
  def right = throw new RuntimeException("no right")

  override def toString = "Nil"
}


