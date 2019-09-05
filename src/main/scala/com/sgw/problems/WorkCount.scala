package com.sgw.problems

object WorkCount {

  def wordCount(text: String): List[(String, Int)] = {
    val cleanString: String = text.toLowerCase.filter { c =>
      c.isLetterOrDigit || c.isWhitespace
    }

    cleanString.split("\\s").filter { word =>
      word.nonEmpty
    }.foldLeft(Map[String, Int]()) { case (aMap, word) =>
      val newMap = aMap.updated(word, aMap.getOrElse(word, 0) + 1)
      println("---")
      println(aMap)
      println(newMap)
      newMap
    }.toList.sortBy { case (word, count) =>
      (-count, word)
    }
  }

  def wordCount2(text: String): List[(String, Int)] =
    text.toLowerCase.filter { c =>
      c.isLetterOrDigit || c.isWhitespace
    }.split("\\s").filter { word =>
      word.nonEmpty
    }.groupBy { str =>
      str
    }.map { case (word, words) =>
      word -> words.length
    }.toList.sortBy { case (word, count) =>
      (-count, word)
    }

  def main(args: Array[String]): Unit = {
    println("----")
    println(wordCount("A bee is a type of  insect. An insect has 6 legs."))
    println("----")
    println(wordCount2("A bee is a type of  insect. An insect has 6 legs."))
  }
}
