package chess.pieces

import chess.{Board, Move}
import chess.positions.Position
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class KnightSpec extends AnyWordSpec with Matchers {

  "KnightSpec" should {
    val board = Board.createBoard

    "change identifier" in {
      val knight = Knight(Position(1, 0))

      knight.differentiatePlayer mustEqual Knight(Position(1, 0), "N")
    }

    "jump over pieces" in {
      val move = Move(Position(1, 0), Position(2, 2))
      val knight = board(move.from)

      knight.isValidMove(move.from, move.to, board) mustBe true
    }

    "move L shape" in {
      val oneSquare2side = Move(Position(1, 0), Position(0, 2))
      val knight1 = board(oneSquare2side.from)
      val board1 = Board.updateBoard(board, board(oneSquare2side.from), oneSquare2side)

      knight1.isValidMove(oneSquare2side.from, oneSquare2side.to, board) mustBe true

      val twoSide1square = Move(Position(0, 2), Position(2, 3))
      val knight2 = board1(twoSide1square.from)

      knight2.isValidMove(twoSide1square.from, twoSide1square.to, board) mustBe true
    }

    "move vertical" in {
      val move1 = Move(Position(1, 0), Position(2, 2))
      val updatedBoard = Board.updateBoard(board, board(move1.from), move1)

      val move2 = Move(Position(2, 2), Position(2, 4))
      val knight = updatedBoard(move2.from)

      knight.isValidMove(move2.from, move2.to, board) mustBe false
    }

    "move vertical taking own piece" in {
      val move = Move(Position(1, 0), Position(1, 1))
      val knight = board(move.from)

      knight.isValidMove(move.from, move.to, board) mustBe false
    }

    "move horizontal" in {
      val move1 = Move(Position(1, 0), Position(2, 2))
      val updatedBoard = Board.updateBoard(board, board(move1.from), move1)

      val move2 = Move(Position(2, 2), Position(4, 2))
      val knight = updatedBoard(move2.from)

      knight.isValidMove(move2.from, move2.to, board) mustBe false
    }

    "move horizontal taking own piece" in {
      val move = Move(Position(1, 0), Position(2, 0))
      val knight = board(move.from)

      knight.isValidMove(move.from, move.to, board) mustBe false
    }

    "move diagonal" in {
      val move1 = Move(Position(1, 0), Position(2, 2))
      val updatedBoard = Board.updateBoard(board, board(move1.from), move1)

      val move2 = Move(Position(2, 2), Position(4, 4))
      val knight = updatedBoard(move2.from)

      knight.isValidMove(move2.from, move2.to, board) mustBe false
    }

    "move diagonal taking own piece" in {
      val move = Move(Position(1, 0), Position(2, 1))
      val knight = board(move.from)

      knight.isValidMove(move.from, move.to, board) mustBe false
    }

  }
}
