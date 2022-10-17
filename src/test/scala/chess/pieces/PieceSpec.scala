package chess.pieces

import cats.kernel.laws.discipline.EqTests
import chess.FpFinalSpec
import chess.app.Move
import chess.board.Board
import chess.positions.Position
import org.scalacheck.{Arbitrary, Gen}

class PieceSpec extends FpFinalSpec {

  test("validates if piece is not taking own piece") {
    forAll { (pawnMove: Move) =>
      assert(
        Piece
          .isNotTakingOwnPiece(pawnMove, Board.unsafeCreate(Map(pawnMove.from -> pawnMove.piece)), pawnMove.piece.color)
          .isValid
      )
    }
  }

  test("invalidates if piece is taking own piece") {
    forAll { (pawnMove: Move, piece: Piece) =>
      val currentBoard = Board.unsafeCreate(Map(pawnMove.from -> pawnMove.piece, pawnMove.to -> piece))
      assert(
        Piece.isNotTakingOwnPiece(pawnMove, currentBoard, pawnMove.piece.color).isInvalid
      )
    }
  }

  // flaky test, might fail when 'piece.sourcePosition' has either or both file rank close to the board boarders
  test("validates if piece path is empty") {
    forAll { (piece: Piece) =>
      val moveBasedOnPiece =
        Move.unsafeCreate(
          piece,
          piece.sourcePosition,
          Position.unsafeCreate(piece.sourcePosition.file + 1, piece.sourcePosition.rank + 1)
        )
      assert(
        Piece.isPathEmpty(moveBasedOnPiece, Board.unsafeCreate(Map(piece.sourcePosition -> piece))).isValid
      )
    }
  }

  test("invalidates if piece path is not empty") {
    implicit val positionArb: Arbitrary[Position] = Arbitrary {
      for {
        file <- Gen.choose(Board.dimension.start, Board.dimension.end - 3)
        rank <- Gen.choose(Board.dimension.start, Board.dimension.end - 3)
      } yield Position.unsafeCreate(file, rank)
    }

    implicit def arbPiece(implicit arbPos: Arbitrary[Position]): Arbitrary[Piece] = Arbitrary {
      for {
        pos1 <- arbPos.arbitrary
        pos2 <- arbPos.arbitrary
        pos3 <- arbPos.arbitrary
        pieces = List(Bishop(pos1), Queen(pos2), Rook(pos3))
        piece <- Gen.oneOf(pieces)
      } yield piece
    }

    forAll { (piece: Piece) =>
      val moveBasedOnPiece =
        Move.unsafeCreate(
          piece,
          piece.sourcePosition,
          Position.unsafeCreate(piece.sourcePosition.file + 2, piece.sourcePosition.rank)
        )
      val inBetweenPiece = Position.unsafeCreate(piece.sourcePosition.file + 1, piece.sourcePosition.rank)
      val currentBoard =
        Board.unsafeCreate(Map(piece.sourcePosition -> piece, inBetweenPiece -> piece))

      assert(Piece.isPathEmpty(moveBasedOnPiece, currentBoard).isInvalid)
    }
  }

  checkAll("Eq[Piece]", EqTests[Piece].eqv)

}
