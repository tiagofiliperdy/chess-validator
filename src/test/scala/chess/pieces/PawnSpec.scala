package chess.pieces

import cats.implicits._
import chess.FpFinalSpec
import chess.app.Move
import chess.board.Board

class PawnSpec extends FpFinalSpec {

  val kingMoves =
    Table(
      ("move", "isValid"),
      (oneRightMove, false),
      (oneLeftMove, false),
      (oneUpwards, true),
      (oneDownwards, true),
      (oneDiagonalPositiveMove, false),
      (diagonalNegativeMove, false),
      (rightMove, false),
      (leftMove, false),
      (upwards, true),
      (downwards, true),
      (diagonalPositiveMove, false),
      (unorthodoxMove1, false),
      (unorthodoxMove2, false),
      (unorthodoxMove3, false)
    )

  test("pawn can change its identifier") {
    forAll { pawn: Pawn =>
      pawn.differentiatePlayer =!= pawn
    }
  }

  test("validates pawn moves") {
    forAll { pawn: Pawn =>
      forAll(kingMoves) { (move: Piece => Move, isValid: Boolean) =>
        val onePieceBoard = Board.unsafeCreate(Map(pawn.sourcePosition -> pawn))
        val newPiece =
          if (move(pawn) eqv oneDownwards(pawn)) pawn.differentiatePlayer else pawn
        assert(newPiece.isValidMove(move(newPiece), onePieceBoard).isValid == isValid)
      }
    }
  }

}
