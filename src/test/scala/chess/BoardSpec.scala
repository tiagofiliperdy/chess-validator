package chess

import chess.pieces.White
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BoardSpec extends AnyWordSpec with Matchers {

  "BoardSpec" should {

    "create a board" in {
      val board = Board.createBoard

      board.keySet.size mustBe 32
      board.values.count(p => p.color.equals(White)) mustBe 16
    }

    "update the board" in {
      val board = Board.createBoard
      val move = Move(List(4, 6, 4, 4))
      val piece = board(move.from)
      val updatedBoard = Board.updateBoard(board, piece, move)

      board.get(move.to) mustBe None
      updatedBoard.get(move.to).isDefined mustBe true
    }
  }
}
