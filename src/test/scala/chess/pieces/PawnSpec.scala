package chess.pieces

import chess.{Board, Move}
import chess.positions.Position
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PawnSpec extends AnyWordSpec with Matchers {

  "PawnSpec" should {
    val initBoard = Board.createBoard

    "change identifier" in {
      val pawn = Pawn(Position(0, 6))

      pawn.differentiatePlayer mustEqual Pawn(Position(0, 6), "P")
    }

    "first move with 1 square" in {
      val move = Move(Position(0, 6), Position(0, 5))
      val pawn = initBoard(move.from)

      pawn.isValidMove(move.from, move.to, initBoard) mustBe true
    }

    "first move with 2 square" in {
      val move = Move(Position(0, 6), Position(0, 4))
      val pawn = initBoard(move.from)

      pawn.isValidMove(move.from, move.to, initBoard) mustBe true
    }

    "move with more than 1 vertical square" in {
      val move1 = Move(Position(0, 6), Position(0, 4))
      val board2 = Board.updateBoard(initBoard, initBoard(move1.from), move1)

      val move2 = Move(Position(0, 4), Position(0, 2))
      val pawn = board2(move2.from)

      pawn.isValidMove(move2.from, move2.to, board2) mustBe false
    }

    "move diagonally with no piece to take" in {
      val move = Move(Position(1, 6), Position(2, 5))
      val pawn = initBoard(move.from)

      pawn.isValidMove(move.from, move.to, initBoard) mustBe false
    }

    "move more than one square diagonally" in {
      val move = Move(Position(1, 6), Position(3, 4))
      val pawn = initBoard(move.from)

      pawn.isValidMove(move.from, move.to, initBoard) mustBe false
    }

    "move diagonally with a piece to take" in {
      val play1move1 = Move(Position(1, 6), Position(1, 4))
      val board1 = Board.updateBoard(initBoard, initBoard(play1move1.from), play1move1)

      val play2move1 = Move(Position(2, 1), Position(2, 3))
      val board2 = Board.updateBoard(board1, board1(play2move1.from), play2move1)

      val play1move2 = Move(Position(1, 4), Position(2, 3))
      val pawn = board2(play1move2.from)

      pawn.isValidMove(play1move2.from, play1move2.to, board2) mustBe true
    }

    "move backwards" in {
      val move1 = Move(Position(1, 6), Position(1, 4))
      val board1 = Board.updateBoard(initBoard, initBoard(move1.from), move1)

      val move2 = Move(Position(1, 4), Position(1, 5))
      val pawn = board1(move2.from)

      pawn.isValidMove(move2.from, move2.to, board1) mustBe false
    }
  }

}
