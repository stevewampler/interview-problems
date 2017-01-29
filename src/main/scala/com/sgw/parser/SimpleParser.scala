package com.sgw.parser

import scala.util.parsing.combinator._

// an example of scala-parser-combinators
// see https://github.com/scala/scala-parser-combinators/blob/1.0.x/docs/Getting_Started.md
object SimpleParser extends RegexParsers {
  def word: Parser[String]   = """[a-z]+""".r       ^^ { _.toString }
  def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def freq: Parser[WordFreq] = word ~ number        ^^ { case wd ~ fr => WordFreq(wd,fr) }

  case class WordFreq(word: String, count: Int) {
    override def toString = "Word <" + word + "> " +
      "occurs with frequency " + count
  }

  def main(args: Array[String]) = {
    parse(freq, "fred 123") match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _) => println("FAILURE: " + msg)
      case Error(msg, _) => println("ERROR: " + msg)
    }
  }
}