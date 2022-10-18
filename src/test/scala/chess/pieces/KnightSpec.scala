package chess.pieces

import cats.implicits._
import chess.FpFinalSpec
import chess.app.Move
import chess.board.Board
import chess.positions.Position
import org.scalacheck.{Arbitrary, Gen}

class KnightSpec extends FpFinalSpec {

  implicit val customPositionArb: Arbitrary[Position] = Arbitrary {
    for {
      file <- Gen.choose(Board.dimension.start + 2, Board.dimension.end - 2)
      rank <- Gen.choose(Board.dimension.start + 2, Board.dimension.end - 2)
    } yield Position.unsafeCreate(file, rank)
  }

  val knightMoves =
    Table(
      ("move", "isValid"),
      (oneRightMove, false),
      (oneLeftMove, false),
      (oneUpwards, false),
      (oneDownwards, false),
      (oneDiagonalPositiveMove, false),
      (diagonalNegativeMove, false),
      (rightMove, false),
      (leftMove, false),
      (upwards, false),
      (downwards, false),
      (diagonalPositiveMove, false),
      (unorthodoxMove1, true),
      (unorthodoxMove2, true),
      (unorthodoxMove3, true)
    )

  test("knight can change its identifier") {
    forAll { knight: Knight =>
      knight.differentiatePlayer =!= knight
    }
  }

  test("validates knight moves") {
    forAll { knight: Knight =>
      forAll(knightMoves) { (move: Knight => Move, isValid: Boolean) =>
        val onePieceBoard = Board.unsafeCreate(Map(knight.sourcePosition -> knight))

        assert(knight.isValidMove(move(knight), onePieceBoard).isValid == isValid)
      }
    }
  }

  test("knight can jump over pieces") {
    forAll { knight: Knight =>
      val secondPiece =
        Pawn(Position.unsafeCreate(knight.sourcePosition.file + 1, knight.sourcePosition.rank))
      val board =
        Board.unsafeCreate(Map(knight.sourcePosition -> knight, secondPiece.sourcePosition -> secondPiece))
      val move = Move.unsafeCreate(
        knight,
        knight.sourcePosition,
        Position.unsafeCreate(knight.sourcePosition.file + 1, knight.sourcePosition.rank + 2)
      )

      assert(knight.isValidMove(move, board).isValid)
    }
  }

}
