package chess.pieces

import chess.{Board, Move}
import chess.positions.Position
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BishopSpec extends AnyWordSpec with Matchers {

  "BishopSpec" should {
    val initBoard = Board.createBoard
    val movePawn = Move(Position(3, 6), Position(3, 4))
    val board = Board.updateBoard(initBoard, initBoard(movePawn.from), movePawn)

    "change identifier" in {
      val bishop = Bishop(Position(2, 7))

      bishop.differentiatePlayer mustEqual Bishop(Position(2, 7), "B")
    }

    "get positions in between diagonal move" in {
      val move = Move(Position(2, 7), Position(6, 3))
      val bishop = board(move.from)

      val filesToCheck = bishop.inBetweenCoordinates(move.from.file, move.to.file)
      val ranksToCheck = bishop.inBetweenCoordinates(move.from.rank, move.to.rank)

      bishop.zipCoordinates(filesToCheck, ranksToCheck) mustEqual List((3, 6), (4, 5), (5, 4))
    }

    "move vertical" in {
      val move1 = Move(Position(2, 6), Position(2, 4))
      val afterPawnMove = Board.updateBoard(board, board(move1.from), move1)

      val moveVertical = Move(Position(2, 7), Position(2, 5))
      val bishop = afterPawnMove(moveVertical.from)

      bishop.isValidMove(moveVertical.from, moveVertical.to, afterPawnMove) mustBe false
    }

    "move vertical and take own piece" in {
      val move = Move(Position(2, 7), Position(2, 6))
      val bishop = board(move.from)

      bishop.isValidMove(move.from, move.to, board) mustBe false
    }

    "move horizontal" in {
      val move1 = Move(Position(2, 7), Position(4, 5))
      val updatedBoard = Board.updateBoard(board, board(move1.from), move1)

      val moveHorizontal = Move(Position(4, 5), Position(2, 5))
      val bishop = updatedBoard(moveHorizontal.from)

      bishop.isValidMove(moveHorizontal.from, moveHorizontal.to, updatedBoard) mustBe false
    }

    "move horizontal and take own piece" in {
      val move = Move(Position(2, 7), Position(2, 6))
      val bishop = board(move.from)

      bishop.isValidMove(move.from, move.to, board) mustBe false
    }

    "move diagonal" in {
      val move = Move(Position(2, 7), Position(4, 5))
      val bishop = board(move.from)

      bishop.isValidMove(move.from, move.to, board) mustBe true
    }

    "move diagonal and take own piece" in {
      val move1 = Move(Position(2, 7), Position(4, 5))
      val updatedBoard = Board.updateBoard(board, board(move1.from), move1)

      val move2 = Move(Position(4, 5), Position(5, 6))
      val bishop = updatedBoard(move2.from)

      bishop.isValidMove(move2.from, move2.to, updatedBoard) mustBe false
    }
  }

}
