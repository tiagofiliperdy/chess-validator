package chess.pieces

import chess.positions.Position
import chess.{Board, Move}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class QueenSpec extends AnyWordSpec with Matchers {

  "QueenSpec" should {
    val initBoard = Board.createBoard
    val movePawn = Move(Position(3, 6), Position(3, 4))
    val board = Board.updateBoard(initBoard, initBoard(movePawn.from), movePawn)

    "change identifier" in {
      val queen = Queen(Position(3, 7))

      queen.differentiatePlayer mustEqual Queen(Position(3, 7), "Q")
    }

    "get positions in between vertical move" in {
      val move = Move(Position(3, 7), Position(3, 5))
      val queen = board(move.from)

      val filesToCheck = queen.inBetweenCoordinates(move.from.file, move.to.file)
      val ranksToCheck = queen.inBetweenCoordinates(move.from.rank, move.to.rank)

      queen.zipCoordinates(filesToCheck, ranksToCheck) mustEqual List((3, 6))
    }

    "move vertical" in {
      val move = Move(Position(3, 7), Position(3, 5))
      val queen = board(move.from)

      queen.isValidMove(move.from, move.to, board) mustBe true
    }

    "move vertical and take own piece" in {
      val move = Move(Position(3, 7), Position(3, 4))
      val queen = board(move.from)

      queen.isValidMove(move.from, move.to, board) mustBe false
    }

    "move horizontal" in {
      val move1 = Move(Position(3, 7), Position(3, 5))
      val updatedBoard = Board.updateBoard(board, board(move1.from), move1)

      val moveHorizontal = Move(Position(3, 5), Position(0, 5))
      val queen = updatedBoard(moveHorizontal.from)

      queen.isValidMove(moveHorizontal.from, moveHorizontal.to, updatedBoard) mustBe true
    }

    "move horizontal and take own piece" in {
      val move = Move(Position(3, 7), Position(2, 7))
      val queen = board(move.from)

      queen.isValidMove(move.from, move.to, board) mustBe false
    }

    "move diagonal" in {
      val move1 = Move(Position(3, 7), Position(3, 6))
      val updatedBoard = Board.updateBoard(board, board(move1.from), move1)

      val move2 = Move(Position(3, 6), Position(0, 3))
      val queen = updatedBoard(move2.from)

      queen.isValidMove(move2.from, move2.to, updatedBoard) mustBe true
    }

    "move diagonal and take own piece" in {
      val move = Move(Position(3, 7), Position(4, 6))
      val queen = board(move.from)

      queen.isValidMove(move.from, move.to, board) mustBe false
    }
  }
}
