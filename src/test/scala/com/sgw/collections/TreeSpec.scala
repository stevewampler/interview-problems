package com.sgw.collections

import com.sgw.collections.immutable.{INode, Node}
import org.scalatest.BeforeAndAfter
import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * author: steve
 *
 * 0 5 shallow
 *   6 7
 * 1 8 9 10
 * 2 11
 *   12
 *   13 14
 * 3 15 16
 *      17 18
 * 4 19 20 21
 *         22
 * deep
 */
class TreeSpec extends FlatSpec with Matchers with BeforeAndAfter {
  var tree: INode[Int] = _

  val deepNode = Node(-1)
  val shallowNode = Node(-1)

  before {
    tree = Node(
      0,
      left = Node(
        1,
        left = Node(
          2,
          left =Node(
            3,
            left = Node(
              4,
              left = deepNode,
              right = Node(
                19,
                right = Node(
                  20, right = Node(
                    21,
                    Node(22)
                  )
                )
              )
            ),
            right = Node(
              15,
              right=Node(
                16,
                left = Node(
                  17,
                  right = Node(18)
                )
              )
            )
          ),
          right = Node(
            11,
            left = Node(
              12,
              left = Node(
                13,
                right = Node(14)
              )
            )
          )
        ),
        right = Node(
          8,
          right = Node(
            9,
            right = Node(
              10
            )
          )
        )
      ),
      right = Node(
        5,
        left = Node(
          6,
          right = Node(
            7
          )
        ),
        right = shallowNode
      )
    )
  }

  "A tree" should "be constructable" in {
    println(tree)
  }

  "The tree breadthFirstSearch method" should "find something breadth first" in {
    val nodeOpt = tree.breadthFirstSearch(-1)

    nodeOpt.isDefined should be (right=true)

    nodeOpt.get should be (shallowNode)
    nodeOpt.get.value should be (-1)
  }

  "The tree depthFirstSearch method" should "find something depth first" in {
    val nodeOpt = tree.depthFirstSearch(-1)

    nodeOpt.isDefined should be (right=true)

    nodeOpt.get should be (deepNode)
    nodeOpt.get.value should be (-1)
  }
}
