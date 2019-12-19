package chess.pieces

import chess.{Board, Move}
import chess.positions.Position
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class KingSpec extends AnyWordSpec with Matchers {

  "KingSpec" should {
    val initBoard = Board.createBoard
    val movePawn = Move(Position(4, 1), Position(4, 3))
    val board = Board.updateBoard(initBoard, initBoard(movePawn.from), movePawn)

    "change identifier" in {
      val king = King(Position(4, 0))

      king.differentiatePlayer mustEqual King(Position(4, 0), "K")
    }

    "move vertical" in {
      val move = Move(Position(4, 0), Position(4, 1))
      val king = board(move.from)

      king.isValidMove(move.from, move.to, board) mustBe true
    }

    "move vertical and take own piece" in {
      val auxBoard = Board.createBoard
      val move = Move(Position(4, 0), Position(4, 1))
      val king = board(move.from)

      king.isValidMove(move.from, move.to, auxBoard) mustBe false
    }

    "move horizontal" in {
      val move1 = Move(Position(4, 0), Position(4, 1))
      val updatedBoard1 = Board.updateBoard(board, board(move1.from), move1)

      val move2 = Move(Position(4, 1), Position(4, 2))
      val updatedBoard2 = Board.updateBoard(updatedBoard1, updatedBoard1(move2.from), move2)

      val moveHorizontal = Move(Position(4, 2), Position(3, 2))
      val king = updatedBoard2(moveHorizontal.from)

      king.isValidMove(moveHorizontal.from, moveHorizontal.to, updatedBoard2) mustBe true
    }

    "move horizontal and take own piece" in {
      val move = Move(Position(4, 0), Position(3, 0))
      val king = board(move.from)

      king.isValidMove(move.from, move.to, board) mustBe false
    }

    "move diagonal" in {
      val move1 = Move(Position(4, 0), Position(4, 1))
      val updatedBoard1 = Board.updateBoard(board, board(move1.from), move1)

      val move2 = Move(Position(4, 1), Position(3, 2))
      val king = updatedBoard1(move2.from)

      king.isValidMove(move2.from, move2.to, updatedBoard1) mustBe true
    }

    "move diagonal and take own piece" in {
      val move = Move(Position(4, 0), Position(3, 1))
      val king = board(move.from)

      king.isValidMove(move.from, move.to, board) mustBe false
    }

    "move only one square" in {
      val move = Move(Position(4, 0), Position(4, 2))
      val king = board(move.from)

      king.isValidMove(move.from, move.to, board) mustBe false
    }
  }
}
