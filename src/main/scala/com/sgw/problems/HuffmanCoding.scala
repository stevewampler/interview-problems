package com.sgw.problems

import scala.collection.mutable

object HuffmanCoding {
  private sealed trait MyNode {
    def name: String
    def count: Int
  }

  private implicit val nodeOrdering = Ordering.by[MyNode, Int] { node => -node.count }

  private case class SymbolNode(char: Char, count: Int) extends MyNode {
    override def name: String = char.toString

    override def toString: String = s"$name: $count"
  }

  private case class InternalNode(left: MyNode, right: MyNode) extends MyNode {
    override lazy val name: String = left.name + right.name
    override lazy val count: Int = left.count + right.count

    override def toString: String = s"$name: $count\nleft=$left\nright=$right"
  }

  private def generateEncodingMap(root: MyNode): Map[Char, String] = {
    val queue = mutable.Queue[(MyNode, String)]()
    val map = mutable.Map[Char, String]()

    queue.enqueue((root, ""))

    while (queue.nonEmpty) {
      val pair = queue.dequeue()
      val path = pair._2

      pair._1 match {
        case node: InternalNode =>
          queue.enqueue((node.left, path + "0"))
          queue.enqueue((node.right, path + "1"))
        case node: SymbolNode =>
          map.update(node.char, path)
      }
    }

    map.toMap
  }

  private def generateEncodingTree(str: String): MyNode = {
    val pq = str.foldLeft(Map[Char, Int]()) { case (acc, c) =>
      acc.updated(c, acc.getOrElse(c, 0) + 1)
    }.map { case (c, count) =>
      SymbolNode(c, count)
    }.toList.foldLeft(new mutable.PriorityQueue) { case (acc, node) =>
      acc.enqueue(node)
      acc
    }

    println(pq)

    while (pq.size > 1) {
      val n1 = pq.dequeue()
      val n2 = pq.dequeue()
      pq.enqueue(InternalNode(n1, n2))
    }

    val root: MyNode = pq.dequeue()

    println(root)

    root
  }

  def encode(str: String, encodingMap: Map[Char, String]): String = {
    str.foldLeft(new mutable.StringBuilder()) { case (builder, c) =>
      builder.append(encodingMap.getOrElse(c, throw new RuntimeException("Unknown character '$c'!")))
    }.toString()
  }

  def decode(encodedString: String, root: MyNode): String = {
    encodedString.foldLeft((new mutable.StringBuilder(), root)) {
      case ((strBuilder, node), zeroOrOne) => node match {
        case iNode: InternalNode =>
          val childNode = if (zeroOrOne == '0') {
            iNode.left
          } else {
            iNode.right
          }

          childNode match {
            case _: InternalNode => (strBuilder, childNode)
            case symbolNode: SymbolNode => (strBuilder.append(symbolNode.char), root)
          }
        case symbolNode: SymbolNode =>
          (strBuilder.append(symbolNode.char), root)
      }
    }._1.toString()
  }

  def main(args: Array[String]): Unit = {
    val str = "A_DEAD_DAD_CEDED_A_BAD_BABE_A_BEADED_ABACA_BED"

    println(str)

    val encodingTree = generateEncodingTree(str)

    val encodingMap = generateEncodingMap(encodingTree)

    val encodedString = encode(str, encodingMap)

    println(encodedString)

    val decodedString = decode(encodedString, encodingTree)

    println(decodedString)
  }
}
