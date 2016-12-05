package com.sgw.problems

import scala.annotation.tailrec

/**
  * Write a function to generate Excel column labels:
  *
  * A B C ... Z AA AB AC ... AZ BA BB BC ... BZ
  */
object ExcelColumnLabels {
  @tailrec
  def toColumnLabel(columnNumber: Int, label: String = ""): String = {
    if (columnNumber == 0) return label

    val columnIndex = columnNumber - 1

    toColumnLabel(columnIndex / 26, toChar(columnIndex % 26) + label)
  }

  private def toChar(i: Int): Char = ('A' + i).toChar

  def main(args: Array[String]) {
    (1 to 1000).foreach(i => println(s"$i ${toColumnLabel(i)}"))
  }
}
