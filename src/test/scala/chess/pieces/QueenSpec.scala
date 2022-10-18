package chess.pieces

import chess.FpFinalSpec
import cats.implicits._
import chess.app.Move
import chess.board.Board
import chess.positions.Position
import org.scalacheck.{Arbitrary, Gen}

class QueenSpec extends FpFinalSpec {

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
      (diagonalPositiveMove, true),
      (diagonalNegativeMove, true),
      (unorthodoxMove1, false),
      (unorthodoxMove2, false),
      (unorthodoxMove3, false)
    )

  test("queen can change its identifier") {
    forAll { queen: Queen =>
      queen.differentiatePlayer =!= queen
    }
  }

  test("validates queen moves") {
    forAll { queen: Queen =>
      forAll(queenMoves) { (move: Queen => Move, isValid: Boolean) =>
        val onePieceBoard = Board.unsafeCreate(Map(queen.sourcePosition -> queen))

        assert(queen.isValidMove(move(queen), onePieceBoard).isValid == isValid)
      }
    }
  }

}
