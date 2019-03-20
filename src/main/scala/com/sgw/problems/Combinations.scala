package com.sgw.problems

object Combinations {
  def combinations(str: String): Set[String] = {
    if (str.isEmpty) return Set()

    val head = str.head
    val tail = str.tail

//    println(s"head=$head tail=$tail")

    val tailCombos = combinations(tail)

    val headPlusTailCombos = tailCombos.map { tailCombo =>
      s"$head$tailCombo"
    }

    tailCombos ++ headPlusTailCombos + head.toString
  }

  def main(args: Array[String]): Unit = {
    {
      val expected = Set()
      val actual = combinations("")

//      println(expected)
//      println(actual)

      assert(actual == expected)
    }

    {
      val expected = Set("A")
      val actual = combinations("A")

//      println(expected)
//      println(actual)

      assert(actual == expected)
    }

    {
      val expected = Set(
        "A",
        "AB",
        "ABC",
        "ABCD",
        "ABD",
        "AC",
        "ACD",
        "AD",
        "B",
        "BC",
        "BCD",
        "BD",
        "C",
        "CD",
        "D"
      )
      val actual = combinations("ABCD")

      println(expected)
      println(actual)

      assert(actual == expected)
    }
  }
}
