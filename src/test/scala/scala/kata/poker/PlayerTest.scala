package scala.kata.poker

import org.scalatest.{FunSpec, ShouldMatchers}

/**
  * Created by chengl on 4/12/15.
  */
class PlayerTest extends FunSpec with ShouldMatchers {
  describe("A Player") {
    describe("when given Black: 2H 3D 5S 9C KD") {
      val player = Player("Black: 2H 3D 5S 9C KD")

      it("should have name Black") {
        player.name should be("Black")
      }

      it("should have the correct hand") {
        player.hand should be(Hand("2H 3D 5S 9C KD"))
      }
    }
  }
}
