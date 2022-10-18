package chess.pieces

import chess.FpFinalSpec
import chess.board.Board
import chess.positions.Position
import org.scalacheck.{Arbitrary, Gen}
import cats.implicits._
import chess.app.Move

class RookSpec extends FpFinalSpec {

  implicit val customPositionArb: Arbitrary[Position] = Arbitrary {
    for {
      file <- Gen.choose(Board.dimension.start + 2, Board.dimension.end - 2)
      rank <- Gen.choose(Board.dimension.start + 2, Board.dimension.end - 2)
    } yield Position.unsafeCreate(file, rank)
  }

  val queenMoves =
    Table(
      ("move", "isValid"),
      (rightMove, true),
      (leftMove, true),
      (upwards, true),
      (downwards, true),
      (diagonalPositiveMove, false),
      (diagonalNegativeMove, false),
      (unorthodoxMove1, false),
      (unorthodoxMove2, false),
      (unorthodoxMove3, false)
    )

  test("queen can change its identifier") {
    forAll { rook: Rook =>
      rook.differentiatePlayer =!= rook
    }
  }

  test("validates queen moves") {
    forAll { rook: Rook =>
      forAll(queenMoves) { (move: Rook => Move, isValid: Boolean) =>
        val onePieceBoard = Board.unsafeCreate(Map(rook.sourcePosition -> rook))

        assert(rook.isValidMove(move(rook), onePieceBoard).isValid == isValid)
      }
    }
  }

}
