package com.sgw.problems

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

/**
 * author: steve
 */
class DeckSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "A deck's shuffle method" should "shuffle a deck of cards" in {
    val deck = Deck.create

    deck.cards.size should be (52)

    deck.cards.foreach(println)

    val shuffledDeck = deck.shuffle

    shuffledDeck.cards.size should be (52)

    shuffledDeck.cards.foreach(println)
  }
}
