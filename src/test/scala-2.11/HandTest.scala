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
}
