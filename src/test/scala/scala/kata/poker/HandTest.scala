package scala.kata.poker

import org.scalatest.{FunSpec, ShouldMatchers}

class HandTest extends FunSpec with ShouldMatchers {
  describe("A Poker Hand") {
    describe("when given 2H 3D 5S 9C KD") {
      val hand = Hand("2H 3D 5S 9C KD")

      it("should have 5 cards") {
        hand.cards.size should be(5)
      }

      it("should have cards: 2H 3D 5S 9C KD") {
        hand.cards should contain(Card("2H"))
        hand.cards should contain(Card("3D"))
        hand.cards should contain(Card("5S"))
        hand.cards should contain(Card("9C"))
        hand.cards should contain(Card("KD"))
      }

      it("should be High Card") {
        hand.categorizedHand should be(HighCard(Seq(Card("KD"), Card("9C"), Card("5S"), Card("3D"), Card("2H"))))
      }
    }

    describe("when given 2S 3S 4S 5S 6S") {
      it("should be StraightFlush") {
        Hand("2S 3S 4S 5S 6S").categorizedHand should be(StraightFlush(Seq(Card("6S"))))
      }
    }

    describe("when given 2S 2D 2C 2H 5S") {
      it("should be Four of Kind") {
        Hand("2S 2D 2C 2H 5S").categorizedHand should be(FourOfKind(Seq(Card("2H"))))
      }
    }

    describe("when given 2S AS QS 3S 5S") {
      it("should be Flush") {
        Hand("2S AS QS 3S 5S").categorizedHand should be(Flush(Seq(Card("AS"), Card("QS"), Card("5S"), Card("3S"), Card("2S"))))
      }
    }

    describe("when given 2S 5D 4S 6C 3H") {
      it("should be Straight") {
        Hand("2S 5D 4S 6C 3H").categorizedHand should be(Straight(Seq(Card("6C"))))
      }
    }

    describe("when given 2S QD QS QC 3H") {
      it("should be ThreeOfKindBase") {
        Hand("2S QD QS QC 3H").categorizedHand should be(ThreeOfKind(Seq(Card("QC"))))
      }
    }

    describe("when given 2S QD QS 2C 3H") {
      it("should be Two Pairs") {
        Hand("2S QD QS 2C 3H").categorizedHand should be(TwoPairs(Seq(Card("QD"), Card("QS"), Card("2S"), Card("2C"), Card("3H"))))
      }
    }

    describe("when given 2S QD AS 2C 3H") {
      it("should be One Pair") {
        Hand("2S QD AS 2C 3H").categorizedHand should be(OnePair(Seq(Card("2C"), Card("2S"), Card("AS"), Card("QD"), Card("3H"))))
      }
    }
  }
}