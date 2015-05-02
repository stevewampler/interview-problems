package com.sgw.problems

object Suit extends Enumeration {
  type Suit = Value
  val Clubs, Diamonds, Hearts, Spades = Value
}
import com.sgw.problems.Suit._

case class Card(value: Int, suit: Suit) {
  override def toString(): String = {
    val valueStr = value match {
      case 1 => "Ace"
      case x if 2 to 10 contains x => x.toString
      case 11 => "Jack"
      case 12 => "Queen"
      case 13 => "King"
    }

    valueStr + " of " + suit
  }
}

object Deck {
  def create: Deck = Deck(
    Suit.values.foldLeft(List[Card]()) {
      case (deck2, suit) => (1 to 13).foldLeft(deck2) {
        case (deck3, value) => Card(value, suit) :: deck3
      }
    }
  )
}

case class Deck(cards: List[Card]) {
  def shuffle: Deck = Deck(cards.sortBy(_ => Math.random()))
}
