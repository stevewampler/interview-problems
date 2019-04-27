package com.sgw.collections

/**
  * A data structure used to perform auto-completions on strings.
  *
  * See: https://en.wikipedia.org/wiki/Trie
  *
  * @param strs the list of auto-complete strings
  */
case class Trie(strs: List[String]) {

  /**
    * A node in a Trie.
    *
    * @param children a map containing the children of this node. Each entry's key is the next character in the
    *                 child word and each value is another node (branch) containing child completions.
    * @param stop an indicator of whether or not this node represents the end of a word even if it has children
    */
  private case class Node(children: Map[Char, Node] = Map[Char, Node](), stop: Boolean = false) {
    def add(str: String): Node = {
      if (str.isEmpty) return copy(stop = true)

      val c = str.charAt(0)

      val childNode = children.getOrElse(c, Node()).add(str.tail)

      copy(children = children.updated(c, childNode))
    }

    def findNode(prefix: String): Option[Node] = {
      if (prefix.isEmpty) return Some(this)

      children.get(prefix.charAt(0)).flatMap { node =>
        node.findNode(prefix.tail)
      }
    }

    def complete(prefix: String, list: List[String] = List.empty[String]): List[String] = {
      if (children.isEmpty) {
        return prefix :: list
      }

      val list2 = if (stop) {
        prefix :: list
      } else {
        list
      }

      children.foldLeft(list2) { case (acc, (c, child)) =>
        child.complete(prefix = s"$prefix$c", acc)
      }
    }

    override def toString: String = children.toString()
  }

  private val root = strs.foldLeft(Node()) { case (node, str) =>
    node.add(str)
  }

  def complete(prefix: String): List[String] = root.findNode(prefix).map { node =>
    node.complete(prefix)
  }.getOrElse(List.empty[String]).sorted

  override def toString: String = s"Trie(${root.toString})"
}
