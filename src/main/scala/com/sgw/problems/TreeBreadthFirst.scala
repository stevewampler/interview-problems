package com.sgw.problems

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Node {
  def apply(value: Int, left: Node = null, right: Node = null) = new Node(value, Option(left), Option(right))
  def apply(value: Int, leftOpt: Option[Node], rightOpt: Option[Node]) = new Node(value, leftOpt, rightOpt)
}

class Node(val value: Int, val maybeLeft: Option[Node], val maybeRight: Option[Node])

/**
 * Returns a string representing the values of the nodes of a binary tree in breadth-first order.
 *
 * The key is to use a fifo queue to queue up the nodes in the correct order.
 */
object TreeBreadthFirst {
  def breadthFirst(node: Node): String = breadthFirstImpl(-1, Queue[(Int, Node)]( (0, node) ), new StringBuilder())

  @tailrec
  private def breadthFirstImpl(currentLevel: Int, queue: Queue[(Int, Node)], builder: StringBuilder): String = {
    // if the queue is empty, we're done
    if (queue.isEmpty) return builder.toString()

    // dequeue the first node and its level in the tree
    val ((level, node), newQueue) = queue.dequeue

    if (level == currentLevel) {
      builder.append(' ')
    } else if (level != 0) {
      builder.append('\n')
    }

    builder.append(node.value)

    val leftQueue  = node.maybeLeft.map(childNode => newQueue.enqueue( (level + 1, childNode) )).getOrElse(newQueue)
    val rightQueue = node.maybeRight.map(childNode => leftQueue.enqueue( (level + 1, childNode) )).getOrElse(leftQueue)

    breadthFirstImpl(level, rightQueue, builder)
  }
}
