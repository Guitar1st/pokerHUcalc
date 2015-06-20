/**
 * Main module for poker hands
 */

sealed trait Suit

object Suit {
  case object Clubs extends Suit
  case object Spades extends Suit
  case object Hearts extends Suit
  case object Diamonds extends Suit
}

sealed abstract class CardValue(val number: Int)

object CardValue {
  case object Two extends CardValue(2)
  case object Three extends CardValue(3)
  case object Four extends CardValue(4)
  case object Five extends CardValue(5)
  case object Six extends CardValue(6)
  case object Seven extends CardValue(7)
  case object Eight extends CardValue(8)
  case object Nine extends CardValue(9)
  case object Ten extends CardValue(10)
  case object Jack extends CardValue(11)
  case object Queen extends CardValue(12)
  case object King extends CardValue(13)
  case object Ace extends CardValue(14)
}

class Card(val value: CardValue, val suit: Suit) extends Ordered[Card] {
  def compare(that: Card): Int = this.value.number - that.value.number
  def sameNumber(that: Card): Boolean = this.value.number == that.value.number
  def sameSuit(that: Card): Boolean = this.suit == that.suit
  def completelySame(that: Card): Boolean = sameSuit(that) & sameNumber(that)
}

abstract class Hand(val order: Int = 0) extends Ordered[Hand] {
  def orderCounted = order
  def compare(that: Hand): Int = this.orderCounted - that.orderCounted
}

class PlayersHand(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card) extends Hand {
  val cards = List(c1, c2, c3, c4, c5)
  val cardsHR = for (c <- cards) yield (c.value, c.suit)
  if (checkSameCards) throw new IllegalArgumentException("hand has same cards")
  def checkSameCards = {
    val checkList = for {
      card_one <- cards
      card_two <- cards
      if card_one != card_two
    } yield card_one completelySame card_two
    checkList.foldLeft(false)(_|_)
  }
  println(checkSameCards)
  println(cardsHR)
  println(orderCounted)
  override def orderCounted = 0 // Todo: write this correctly
}

object Hand {
  case object StraightFlush extends Hand(9)
  case object Four extends Hand(8)
  case object FullHouse extends Hand(7)
  case object Flush extends Hand(6)
  case object Straight extends Hand(5)
  case object Set extends Hand(4)
  case object TwoPair extends Hand(3)
  case object OnePair extends Hand(2)
  case object HighCard extends Hand(1)
}

