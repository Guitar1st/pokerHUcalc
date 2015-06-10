import org.scalatest.FunSuite
/**
 * Created by dmitrii on 08.06.15.
 */
class HandTest extends FunSuite {
  test("method isCorrect works correctly") {
    var thrown = intercept[IllegalArgumentException] {
      new Hand
    }
    assert(thrown.getMessage === "hand length is wrong!")
    thrown = intercept[IllegalArgumentException] {
      new Hand("2d3c")
    }
    assert(thrown.getMessage === "hand length is wrong!")
    thrown = intercept[IllegalArgumentException] {
      new Hand("ud3d4d5d6h")
    }
    assert(thrown.getMessage === "hand value is wrong!")
    thrown = intercept[IllegalArgumentException] {
      new Hand("2d3d4d5p6h")
    }
    assert(thrown.getMessage === "hand suit is wrong!")
    new Hand("2d3dTd5d6d")
  }

  test("method combination() works correctly") {
    val flushHand = new Hand("2d3dTd5d6d")
    assert(flushHand.combination === "FLUSH")

    val notFlushHand = new Hand("2d3dTs5d6d")
    assert(notFlushHand.combination === "SOMETHING_ELSE")
  }
}
