package com.sgw.problems

import scala.io.Source

/**
  * A scala implementation of the CaesarCipher and a method to decoded a Caesar Cipher'd string using quad frequencies.
  *
  * See http://practicalcryptography.com/cryptanalysis/stochastic-searching/cryptanalysis-caesar-cipher/
  */
object CaesarCipher2 {

  def cipher(str: String, key: Int): String = {
    assert(key.abs >= 1 && key.abs <= 25)

    str.toUpperCase.trim.filter { c =>
      c.isLetter
    }.map { c =>
      c.toInt
    }.map { c =>
      c - 'A' + key
    }.map { c =>
      if (c < 0) {
        26 + c
      } else if (c > 25) {
        c - 26
      } else {
        c
      }
    }.map { c =>
      (c + 'A').toChar
    }.foldLeft(new StringBuilder()) { case (builder, c) =>
      builder.append(c)
    }.toString()
  }

  def decipher(str: String, key: Int): String = cipher(str, -key)

  private val quadToFreq: Seq[(String, Double)] = Source.
    fromFile(getClass.getResource("/english_quadgrams.txt").toURI).
    getLines().
    map { line =>
      line.split(' ')
    }.map { case Array(quad, freq) =>
      quad -> freq.toDouble
    }.toSeq

  private val freqSum: Double = quadToFreq.map(_._2).sum
  private val quadToScore: Map[String, Double] = quadToFreq.map { case (quad, freq) =>
    quad -> Math.log10(freq / freqSum)
  }.toMap
  private val defaultScore = Math.log10(0.01 / freqSum)

  def scoreQuad(quad: String): Double = quadToScore.getOrElse(quad, defaultScore)

  def score(str: String): Double =
    (0 until str.length - 4).map { i =>
      str.substring(i, i + 4)
    }.foldLeft(0.0) { case (score, quad) =>
      score + scoreQuad(quad)
    }

  def decode(str: String): String =
    (1 to 25).map { key =>
      decipher(str, key)
    }.map { decipheredString =>
      decipheredString -> score(decipheredString)
    }.foldLeft(("", Double.MinValue)) { case ((bestString, bestScore), (decipheredString, score)) =>
      if (score > bestScore) {
        (decipheredString, score)
      } else {
        (bestString, bestScore)
      }
    }._1

  def main(args: Array[String]): Unit = {
    val original = "defend the east wall of the castle!"
    val ciphered = cipher(original, key = 15)
    val deciphered = decipher(ciphered, key = 15)
    val decoded = decode(ciphered)

    println(original)
    println(ciphered)
    println(deciphered)
    println(decoded)
  }
}
