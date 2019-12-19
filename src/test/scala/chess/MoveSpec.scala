package chess

import chess.positions.Position
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MoveSpec extends AnyWordSpec with Matchers {

  "MoveSpec" should {

    val move1 = Move(Position(7, 7), Position(9, 7))
    val move2 = Move(Position(7, 7), Position(5, 7))

    "create Move from List[Int]" in {
      val l = List(1, 2, 3, 4)
      val move = Move(l)

      move mustBe (Move(Position(1, 2), Position(3, 4)))
    }

    "validate if position is outside of board dimension" in {
      move1.isToPositionInsideBoard mustBe false
      move2.isToPositionInsideBoard mustBe true
    }

    "print game move on conventional system" in {
      move2.toString mustEqual ("from: h1, to: f1")
    }
  }

}
