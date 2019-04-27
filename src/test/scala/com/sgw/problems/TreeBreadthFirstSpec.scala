package com.sgw.problems

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class TreeBreadthFirstSpec extends FlatSpec with Matchers with BeforeAndAfter {
  import TreeBreadthFirst.Node

  "The TreeBreadthFirst code" should "return a binary tree's nodes in breadth first order" in {
    val tree = Node(10, Node(8, Node(5), Node(9)), Node(11, right = Node(12)))

    val str = TreeBreadthFirst.breadthFirst(tree)

    println(str)

    str should be ("10\n8 11\n5 9 12")
  }
}
