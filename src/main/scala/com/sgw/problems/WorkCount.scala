package com.sgw.problems

object WorkCount {

  def wordCount(text: String): List[(String, Int)] =
    text.toLowerCase.filter { c =>
      c.isLetterOrDigit || c.isWhitespace
    }.split("\\s").filter { word =>
      word.nonEmpty
    }.foldLeft(Map[String, Int]()) { case (acc, word) =>
      acc.updated(word, acc.getOrElse(word, 0) + 1)
    }.toList.sortBy { case (word, count) =>
      (-count, word)
    }

  def main(args: Array[String]): Unit = {
    println(wordCount("A bee is a type of  insect. An insect has 6 legs."))
  }
}
