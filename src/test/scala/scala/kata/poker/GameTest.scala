package scala.kata.poker

import org.scalatest.{FunSpec, ShouldMatchers}

/**
  * Created by chengl on 4/12/15.
  */
class GameTest extends FunSpec with ShouldMatchers {
  describe("A Game") {
    describe("when given Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH") {
      val game = Game("Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH")

      it("should have first player Black: 2H 3D 5S 9C KD") {
        game.player1 should be(Player("Black: 2H 3D 5S 9C KD"))
      }

      it("should have second player White: 2C 3H 4S 8C AH") {
        game.player2 should be(Player("White: 2C 3H 4S 8C AH"))
      }

      it("should have result: White wins with high card A") {
        game.judge should be(Winner("White", "high card A"))
      }
    }

    describe("when given Black: 2H 4S 4C 2D 4H  White: 2S 8S AS QS 3S") {
      val game = Game("Black: 2H 4S 4C 2D 4H  White: 2S 8S AS QS 3S")

      it("should have result: Black wins with FullHouse") {
        game.judge should be(Winner("Black", "FullHouse"))
      }
    }

    describe("when given Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C KH") {
      val game = Game("Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C KH")

      it("should have result: Black wins with high card 9") {
        game.judge should be(Winner("Black", "high card 9"))
      }
    }

    describe("when given Black: 2H 3D 5S 9C KD  White: 2D 3H 5C 9S KH") {
      val game = Game("Black: 2H 3D 5S 9C KD  White: 2D 3H 5C 9S KH")

      it("should have result: Tie") {
        game.judge should be(Tie)
      }
    }

    describe("when given Black: 2H 2D 5S 9C KD  White: 2D 2H 4C 9S KH") {
      val game = Game("Black: 2H 2D 5S 9C KD  White: 2D 2H 4C 9S KH")

      it("should have result: Black wins with high card 5") {
        game.judge should be(Winner("Black", "high card 5"))
      }
    }

    describe("when given Black: 2H 2D 5S 9C KD  White: 3D 3H 4C 9S KH") {
      val game = Game("Black: 2H 2D 5S 9C KD  White: 3D 3H 4C 9S KH")

      it("should have result: White wins with bigger pair 3") {
        game.judge should be(Winner("White", "bigger pair of 3"))
      }
    }

    describe("when given Black: 2H 2D 5S 5C KD  White: 3D 3H 6C 6S KH") {
      val game = Game("Black: 2H 2D 6D 6H KD  White: 3D 3H 6C 6S KH")

      it("should have result: White wins with bigger pair 3") {
        game.judge should be(Winner("White", "bigger pair of 3"))
      }
    }

    describe("when given Black: 2H 2D 2S 5C KD  White: 3D 3H 3C 6S KH") {
      val game = Game("Black: 2H 2D 2S 5C KD  White: 3D 3H 3C 6S KH")

      it("should have result: White wins with bigger three of 3") {
        game.judge should be(Winner("White", "bigger three of 3"))
      }
    }

    describe("when given Black: 2H 4D 3S 5C 6D  White: 7H 6D 3S 5C 4D") {
      val game = Game("Black: 2H 4D 3S 5C 6D  White: 7H 6D 3S 5C 4D")

      it("should have result: White wins with bigger straight of 7") {
        game.judge should be(Winner("White", "bigger straight of 7"))
      }
    }

    describe("when given Black: 2H JH 5H 3H KH  White: TD 7D 9D 3D KD") {
      val game = Game("Black: 2H JH 5H 3H KH  White: TD 7D 9D 3D KD")

      it("should have result: Black wins with bigger flush with high card J") {
        game.judge should be(Winner("Black", "bigger flush with high card J"))
      }
    }

    describe("when given Black: 2H 2D 2S 5C 5D  White: 3D 3H 3C 5S 5H") {
      val game = Game("Black: 2H 2D 2S 5C 5D  White: 3D 3H 3C 5S 5H")

      it("should have result: White wins with bigger three of 3") {
        game.judge should be(Winner("White", "bigger full house with three of 3"))
      }
    }

    describe("when given Black: 2H 2D 2S 2C 5D  White: 3D 3H 3C 3S 5H") {
      val game = Game("Black: 2H 2D 2S 2C 5D  White: 3D 3H 3C 3S 5H")

      it("should have result: White wins with bigger four of 3") {
        game.judge should be(Winner("White", "bigger four of 3"))
      }
    }

    describe("when given Black: 8H JH 9H TH QH  White: AD QD TD JD KD") {
      val game = Game("Black: 8H JH 9H TH QH  White: AD QD TD JD KD")

      it("should have result: White wins with bigger straight flush of A") {
        game.judge should be(Winner("White", "bigger straight flush of A"))
      }
    }
  }
}