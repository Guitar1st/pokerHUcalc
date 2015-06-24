import org.scalatest.FunSuite
/**
 * Main test for Hand and Card classes
 */
class HandTest extends FunSuite {
  test("compare methods for cards work correctly") {
    val card1 = new Card(CardValue.Five, Suit.Hearts)
    val card2 = new Card(CardValue.Six, Suit.Clubs)
    val card3 = new Card(CardValue.Six, Suit.Diamonds)
    val card4 = new Card(CardValue.Ace, Suit.Diamonds)
    val card5 = new Card(CardValue.Ace, Suit.Spades)
    val card6 = new Card(CardValue.Ace, Suit.Spades)

    val hand1 = new PlayersHand(card1, card2, card3, card4, card5)
    val thrown = intercept[IllegalArgumentException] {
      val hand2 = new PlayersHand(card2, card3, card4, card5, card6)
    }
    assert(thrown.getMessage === "hand has same cards")

    assert((card1 < card2) === true)
    assert((card3 < card1) === false)
    assert((card2 <= card3) === true)
    assert((card2 >= card3) === true)
    assert((card2 sameNumber card3) === true)
    assert((card2 sameSuit card3) === false)
    assert((card3 sameSuit card4) === true)
    assert((card3 completelySame card4) === false)
    assert((card5 completelySame card6) === true)
  }

  test("hand order methods work correctly") {
    val card1 = new Card(CardValue.Two, Suit.Hearts)
    val card2 = new Card(CardValue.Three, Suit.Hearts)
    val card3 = new Card(CardValue.Four, Suit.Hearts)
    val card4 = new Card(CardValue.Five, Suit.Hearts)
    val card5 = new Card(CardValue.Six, Suit.Hearts)
    val card6 = new Card(CardValue.Seven, Suit.Hearts)
    val card7 = new Card(CardValue.Eight, Suit.Hearts)
    val card8 = new Card(CardValue.Nine, Suit.Hearts)
    val card9 = new Card(CardValue.Ten, Suit.Hearts)
    val card10 = new Card(CardValue.Jack, Suit.Hearts)
    val card11 = new Card(CardValue.Queen, Suit.Hearts)
    val card12 = new Card(CardValue.King, Suit.Hearts)
    val card13 = new Card(CardValue.Ace, Suit.Hearts)
    val card14 = new Card(CardValue.Ace, Suit.Spades)
    val card15 = new Card(CardValue.King, Suit.Spades)
    val card16 = new Card(CardValue.Queen, Suit.Spades)
    val card17 = new Card(CardValue.Jack, Suit.Spades)
    val card18 = new Card(CardValue.Ten, Suit.Spades)

    val hand1 = new PlayersHand(card1, card2, card3, card4, card5)
    assert(hand1.countForce === new HandForce(Hand.StraightFlush, 2))
    val hand2 = new PlayersHand(card1, card2, card3, card4, card14)
    assert(hand2.countForce === new HandForce(Hand.Straight, 1))
    val hand3 = new PlayersHand(card1, card2, card3, card4, card6)
    assert(hand3.countForce === new HandForce(Hand.Flush, hand3.handId))
    val hand4 = new PlayersHand(card9, card10, card11, card12, card13)
    assert(hand4.countForce === new HandForce(Hand.StraightFlush, 10))
    assert((hand4.countForce > hand1.countForce) === true)
    val hand5 = new PlayersHand(card9, card10, card11, card13, card14)
    assert(hand5.countForce === new HandForce(Hand.OnePair, hand5.handId))
    val hand6 = new PlayersHand(card11, card9, card18, card13, card14)
    assert(hand6.countForce === new HandForce(Hand.TwoPair, hand6.handId))
    assert((hand5.countForce < hand6.countForce) === true)

  }
}
