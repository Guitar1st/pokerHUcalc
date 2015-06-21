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

class PlayersHand(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card) {
  val cards = List(c1, c2, c3, c4, c5)
  val cardsHR = for (c <- cards) yield (c.value, c.suit)
  val cardsNumbersSorted = (for (c <- cards) yield c.value.number).sorted
  if (checkSameCards) throw new IllegalArgumentException("hand has same cards")
  println(cardsHR)
  def checkSameCards = {
    val checkList = for {
        card_one <- cards
        card_two <- cards
      if card_one != card_two
    } yield card_one completelySame card_two
    checkList.foldLeft(false)(_|_)
  }
  def isFlush = {
    val suitList = for (c <- cards) yield c.suit
    suitList.toSet.size == 1
  }
  def isStraight = {
    val lower = cardsNumbersSorted.head
    val straightFromHead = for (j <- 0.to(4)) yield lower + j
    val straightFromAce = List(2, 3, 4, 5, 14)
    cardsNumbersSorted match {
      case `straightFromHead` => lower
      case `straightFromAce` => 1
      case _ => 0
    }
  }
  def force = {
    // todo: finish this func
    lazy val straightVal = isStraight
    if (isFlush) {
      if (straightVal != 0) {new HandForce(Hand.StraightFlush, straightVal)} else Hand.Flush
    } else if (straightVal != 0) {
      new HandForce(Hand.Straight, straightVal)
    } else new HandForce(Hand.HighCard, 0)
  }
}


abstract class Hand(val order: Int = 0)

object Hand {
  case object StraightFlush extends Hand(900)
  case object Four extends Hand(800)
  case object FullHouse extends Hand(700)
  case object Flush extends Hand(600)
  case object Straight extends Hand(500)
  case object Set extends Hand(400)
  case object TwoPair extends Hand(300)
  case object OnePair extends Hand(200)
  case object HighCard extends Hand(100)
}

class HandForce(val handType: Hand, val internalForce: Int = 0) extends Ordered[HandForce] {
  val force = handType.order + internalForce
  def compare(that: HandForce): Int = this.force - that.force
  override def equals(that: Any): Boolean = that match {
    case that: HandForce => this.force == that.force
    case _ => false
  }
}
