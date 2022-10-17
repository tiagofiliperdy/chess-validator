package chess.pieces

import cats.implicits._
import chess.FpFinalSpec
import chess.app.Move
import chess.board.Board
import chess.positions.Position
import org.scalacheck.{Arbitrary, Gen}

class BishopSpec extends FpFinalSpec {

  implicit val customPositionArb: Arbitrary[Position] = Arbitrary {
    for {
      file <- Gen.choose(Board.dimension.start + 2, Board.dimension.end - 2)
      rank <- Gen.choose(Board.dimension.start + 2, Board.dimension.end - 2)
    } yield Position.unsafeCreate(file, rank)
  }

  val bishopMoves =
    Table(
      ("move", "isValid"),
      (rightMove, false),
      (leftMove, false),
      (upwards, false),
      (downwards, false),
      (diagonalPositiveMove, true),
      (diagonalNegativeMove, true),
      (unorthodoxMove1, false),
      (unorthodoxMove2, false),
      (unorthodoxMove3, false)
    )

  test("bishop can change its identifier") {
    forAll { bishop: Bishop =>
      bishop.differentiatePlayer =!= bishop
    }
  }

  test("validates bishop moves") {
    forAll { bishop: Bishop =>
      forAll(bishopMoves) { (move: Bishop => Move, isValid: Boolean) =>
        val onePieceBoard = Board.unsafeCreate(Map(bishop.sourcePosition -> bishop))

        assert(bishop.isValidMove(move(bishop), onePieceBoard).isValid == isValid)
      }
    }
  }

}
