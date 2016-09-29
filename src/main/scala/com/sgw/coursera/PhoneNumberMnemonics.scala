package com.sgw.coursera

import scala.io.Source

/**
  * Derived from the Coursera "Functional Programming Principles in Scala" course.
  */
object PhoneNumberMnemonics extends App {
  /* read a file of words */
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

  /* create a list and filter all words where *all* their characters are not letters (like dashes) */
  val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))

  /* define the map of numbers to letters */
  val nmem = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ"
  ).withDefaultValue("")

  val charCode: Map[Char, Char] = nmem.flatMap {
    case (num, chars) => chars.map(char => (char, num))
  }

  println(charCode)

  def wordCode(word: String): String = word.toUpperCase map charCode

  println(wordCode("ScalaIsGreat"))

  val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()

//  wordsForNum.take(50).foreach(println)

  // generates all of the combination of letters for the specified number
  def encodeCombos(number: String): List[String] = {
    def go(
      number: String,
      string: String = "",
      stringList: List[String] = List()
    ): List[String] = {
      if (number.isEmpty) string :: stringList
      else {
        val head = number.head
        val tail = number.tail
        val chars = nmem(head)
        val foo = chars.flatMap { char => go(tail, string + char, stringList) }.toList
        foo
      }
    }

    go(number)
  }

  encodeCombos("63972278").filter(_ == "NEWSCAST").foreach(println)

  // returns all the ways to encode a number as a list of strings
  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else
      (
        for {
          split <- 1 to number.length
          word <- wordsForNum(number.take(split))
          rest <- encode(number.drop(split))
        } yield word :: rest
      ).toSet

  encode("7225247386").foreach(println)

  def translate(number: String): Set[String] = encode(number).map(list => list.mkString(" "))

  translate("7225247386").foreach(println)
}
