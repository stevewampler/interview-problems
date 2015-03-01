package com.blacklocus.spider

import org.scalatest.{Matchers, FlatSpec}

/**
 * Chad's interview problem in Scala. :)
 */
object WordNumSorter {
  def apply(input: String) = {
    // split the input string into an array
    val list = input.split(" ")

    // map the input list into an array of booleans indicating each item's type (word=true, num=false)
    val types = list.map(isWord)

    // partition the input list by type
    val (wordsAndTypes, numsAndTypes) = list.zip(types).partition {
      case (item, isWord) => isWord
    }

    // isolate the words and sort them
    val words = wordsAndTypes.map {
      case (word, _) => word
    }.sorted

    // isolate the numbers and sort them
    val nums = numsAndTypes.map {
      case (num, _) => num
    }.sorted

    // reduce (foldLeft) to get the sorted-interleaved words and numbers in a list (in reverse order)
    val (result, _, _) = types.foldLeft((List[String](), words, nums)) {
      case ((b, w, n), isWord) => if (isWord)
        (w.head :: b, w.tail, n)
      else
        (n.head :: b, w, n.tail)
    }

    // flip the list around and turn it into a string
    result.reverse.mkString(" ")
  }

  def isWord(item: String) = !isNum(item)
  def isNum(item: String) = item.forall(c => c.isDigit)
}

class WordNumSorterSpec extends FlatSpec with Matchers {
  "The WordNumSorter" should "sort a list of words and numbers leaving words where words were and numbers were numbers were" in {
    val input = "a word or 2 1 will be 7 3 5 enough 0"
    val expected = "a be enough 0 1 or will 2 3 5 word 7"
    val actual = WordNumSorter(input)
    actual should be (expected)
  }
}
