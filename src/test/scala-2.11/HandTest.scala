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
    val card2h = new Card(CardValue.Two, Suit.Hearts)
    val card3h = new Card(CardValue.Three, Suit.Hearts)
    val card4h = new Card(CardValue.Four, Suit.Hearts)
    val card5h = new Card(CardValue.Five, Suit.Hearts)
    val card6h = new Card(CardValue.Six, Suit.Hearts)
    val card7h = new Card(CardValue.Seven, Suit.Hearts)
    val card8h = new Card(CardValue.Eight, Suit.Hearts)
    val card9h = new Card(CardValue.Nine, Suit.Hearts)
    val card10h = new Card(CardValue.Ten, Suit.Hearts)
    val card11h = new Card(CardValue.Jack, Suit.Hearts)
    val card12h = new Card(CardValue.Queen, Suit.Hearts)
    val card13h = new Card(CardValue.King, Suit.Hearts)
    val card14h = new Card(CardValue.Ace, Suit.Hearts)
    val card14s = new Card(CardValue.Ace, Suit.Spades)
    val card13s = new Card(CardValue.King, Suit.Spades)
    val card12s = new Card(CardValue.Queen, Suit.Spades)
    val card11s = new Card(CardValue.Jack, Suit.Spades)
    val card10s = new Card(CardValue.Ten, Suit.Spades)
    val card10c = new Card(CardValue.Ten, Suit.Clubs)
    val card10d = new Card(CardValue.Ten, Suit.Diamonds)

    val hand1 = new PlayersHand(card2h, card3h, card4h, card5h, card6h)
    assert(hand1.countForce === new HandForce(Hand.StraightFlush, 2))
    val hand2 = new PlayersHand(card2h, card3h, card4h, card5h, card14s)
    assert(hand2.countForce === new HandForce(Hand.Straight, 1))
    val hand3 = new PlayersHand(card2h, card3h, card4h, card5h, card7h)
    assert(hand3.countForce === new HandForce(Hand.Flush, hand3.handId))
    val hand4 = new PlayersHand(card10h, card11h, card12h, card13h, card14h)
    assert(hand4.countForce === new HandForce(Hand.StraightFlush, 10))
    assert((hand4.countForce > hand1.countForce) === true)
    val hand5 = new PlayersHand(card10h, card11h, card12h, card14h, card14s)
    assert(hand5.countForce === new HandForce(Hand.OnePair, hand5.handId))
    val hand6 = new PlayersHand(card12h, card10h, card10s, card14h, card14s)
    assert(hand6.countForce === new HandForce(Hand.TwoPair, hand6.handId))
    assert((hand5.countForce < hand6.countForce) === true)
    val hand7 = new PlayersHand(card12h, card10h, card10s, card10c, card14s)
    assert(hand7.countForce === new HandForce(Hand.Set, hand7.handId))
    assert((hand5.countForce > hand7.countForce) === false)
    val hand8 = new PlayersHand(card10d, card10h, card10s, card10c, card14s)
    assert(hand8.countForce === new HandForce(Hand.Four, hand8.handId))
    assert((hand7.countForce > hand8.countForce) === false)
    val hand9 = new PlayersHand(card14h, card10h, card10s, card10c, card14s)
    assert(hand9.countForce === new HandForce(Hand.FullHouse, hand9.handId))
    assert((hand9.countForce > hand8.countForce) === false)

  }
}
