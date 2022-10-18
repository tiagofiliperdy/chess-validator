package chess.pieces

import cats.implicits._
import chess.FpFinalSpec
import chess.app.Move
import chess.board.Board
import chess.positions.Position
import org.scalacheck.{Arbitrary, Gen}

class KingSpec extends FpFinalSpec {

  implicit val customPositionArb: Arbitrary[Position] = Arbitrary {
    for {
      file <- Gen.choose(Board.dimension.start + 2, Board.dimension.end - 2)
      rank <- Gen.choose(Board.dimension.start + 2, Board.dimension.end - 2)
    } yield Position.unsafeCreate(file, rank)
  }

  val kingMoves =
    Table(
      ("move", "isValid"),
      (oneRightMove, true),
      (oneLeftMove, true),
      (oneUpwards, true),
      (oneDownwards, true),
      (oneDiagonalPositiveMove, true),
      (diagonalNegativeMove, true),
      (rightMove, false),
      (leftMove, false),
      (upwards, false),
      (downwards, false),
      (diagonalPositiveMove, false),
      (unorthodoxMove1, false),
      (unorthodoxMove2, false),
      (unorthodoxMove3, false)
    )

  test("king can change its identifier") {
    forAll { king: King =>
      king.differentiatePlayer =!= king
    }
  }

  test("validates king moves") {
    forAll { king: King =>
      forAll(kingMoves) { (move: King => Move, isValid: Boolean) =>
        val onePieceBoard = Board.unsafeCreate(Map(king.sourcePosition -> king))

        assert(king.isValidMove(move(king), onePieceBoard).isValid == isValid)
      }
    }
  }

}
