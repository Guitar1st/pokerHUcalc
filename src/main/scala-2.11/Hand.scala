/**
 * Created by dmitrii on 08.06.15.
 */

object Hand {
  val suits = Seq('h', 'd', 'c', 's')
  val values = Seq('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
  val combinationOrder = Seq(
    "HIGH_CARD",
    "ONE_PAIR",
    "TWO_PAIR",
    "SET",
    "STRAIGHT",
    "FLUSH",
    "FULL_HOUSE",
    "FOUR",
    "STRAIGHT_FLUSH"
  )
  def possibleCards = {
    for {
        i1 <- values
          i2 <- suits
    } yield (i1, i2)
  }
}

class Hand(val cards: String = "") {
  val handSuits = for (i <- cards if cards.indexOf(i) % 2 == 1) yield i
  val handVals = for (i <- cards if cards.indexOf(i) % 2 == 0) yield i
  isCorrect
  def hasCorrectLen = cards.length == 10
  def isCorrect = {
    if (hasCorrectLen) {
      val correctSuits = handSuits.foldLeft(true)((a, b) => a & (Hand.suits contains b))
      val correctVals = handVals.foldLeft(true)((a, b) => a & (Hand.values contains b))
      if (!correctSuits) {
        throw new IllegalArgumentException("hand suit is wrong!")
      } else if (!correctVals) {
        throw new IllegalArgumentException("hand value is wrong!")
      } else true
    } else throw new IllegalArgumentException("hand length is wrong!")
  }
  def combination = {
    val diffSuits = handSuits.toSet
    //val diffVals = handVals.toSet
    println("Different suits:    " +  diffSuits.size.toString)
    if (diffSuits.size == 1) "FLUSH" else "SOMETHING_ELSE"
  }
  def isGreaterThan(otherHand: Hand) {
    println("this hand: " + this.cards)
    println("other hand: " + otherHand.cards)
  }
}
