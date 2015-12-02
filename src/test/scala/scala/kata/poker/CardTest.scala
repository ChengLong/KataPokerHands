package scala.kata.poker

import org.scalatest.{FunSpec, ShouldMatchers}

class CardTest extends FunSpec with ShouldMatchers {
  describe("A card") {
    describe("when it's 2S") {
      val card = Card("2S")

      it("should have suit S") {
        card.suit should be('S')
      }

      it("should have value 2") {
        card.value should be('2')
      }

      it("should have rank 2") {
        card.rank should be(2)
      }

      it("should be < Card TD") {
        card should be < Card("TD")
      }

      it("should be = Card 2H") {
        card should be <= Card("2H")
        card should be >= Card("2H")
      }
    }
  }
}
