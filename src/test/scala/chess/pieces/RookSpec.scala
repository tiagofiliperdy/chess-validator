package chess.pieces

import chess.{Board, Move}
import chess.positions.Position
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RookSpec extends AnyWordSpec with Matchers {

  "RookSpec" should {
    val initBoard = Board.createBoard
    val movePawn = Move(Position(0, 6), Position(0, 4))
    val board = Board.updateBoard(initBoard, initBoard(movePawn.from), movePawn)

    "change identifier" in {
      val rook = Rook(Position(0, 7))

      rook.differentiatePlayer mustEqual Rook(Position(0, 7), "R")
    }

    "move vertical" in {
      val move = Move(Position(0, 7), Position(0, 5))
      val rook = board(move.from)

      rook.isValidMove(move.from, move.to, board) mustBe true
    }

    "move vertical and take own piece" in {
      val move = Move(Position(0, 7), Position(0, 4))
      val rook = board(move.from)

      rook.isValidMove(move.from, move.to, board) mustBe false
    }

    "move horizontal and take own piece" in {
      val move = Move(Position(0, 7), Position(1, 7))
      val rook = board(move.from)

      rook.isValidMove(move.from, move.to, board) mustBe false
    }

    "move horizontal" in {
      val move1 = Move(Position(0, 7), Position(0, 5))
      val afterVerticalMove = Board.updateBoard(board, board(move1.from), move1)

      val moveHorizontal = Move(Position(0, 5), Position(4, 5))
      val rook = afterVerticalMove(moveHorizontal.from)

      rook.isValidMove(moveHorizontal.from, moveHorizontal.to, afterVerticalMove) mustBe true
    }

    "move diagonal" in {
      val move1 = Move(Position(0, 7), Position(0, 5))
      val afterVerticalMove = Board.updateBoard(board, board(move1.from), move1)

      val moveDiagonal = Move(Position(0, 5), Position(2, 3))
      val rook = afterVerticalMove(moveDiagonal.from)

      rook.isValidMove(moveDiagonal.from, moveDiagonal.to, afterVerticalMove) mustBe false
    }
  }

}
