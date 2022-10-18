package chess.board

import cats.data.Validated.Valid
import cats.implicits._
import cats.kernel.laws.discipline.EqTests
import chess.FpFinalSpec
import chess.app.Move
import chess.pieces.{King, Piece, Queen}
import chess.positions.Position
import org.scalacheck.{Arbitrary, Gen}

class BoardSpec extends FpFinalSpec {

  test("gets correct piece") {
    forAll { piece: Piece =>
      val board = Board.unsafeCreate(Map(piece.sourcePosition -> piece))

      assert(board.getPiece(piece.sourcePosition) eqv piece.some)
    }
  }

  test("updates board correctly") {
    forAll { move: Move =>
      val board =
        Board.unsafeCreate(Map(move.piece.sourcePosition -> move.piece, move.to -> move.piece.differentiatePlayer))
      val expectedBoard =
        Board.unsafeCreate(Map(move.to -> move.piece))

      assert(Board.updateBoard(board, move) eqv expectedBoard)
    }
  }

  test("validates if king will be in check") {
    implicit val customKingArb: Arbitrary[King] = Arbitrary {
      for {
        file <- Gen.choose(Board.dimension.start + 2, Board.dimension.end - 3)
        rank <- Gen.choose(Board.dimension.start + 2, Board.dimension.end - 3)
      } yield King(Position.unsafeCreate(file, rank))
    }

    forAll(customKingArb.arbitrary) { (king: King) =>
      val playingPiece = Queen(Position.unsafeCreate(king.sourcePosition.file + 2, king.sourcePosition.rank + 2))
      val menacePiece =
        Queen(Position.unsafeCreate(king.sourcePosition.file + 3, king.sourcePosition.rank + 3)).differentiatePlayer
      val board =
        Board.unsafeCreate(
          Map(
            king.sourcePosition -> king,
            playingPiece.sourcePosition -> playingPiece,
            menacePiece.sourcePosition -> menacePiece
          )
        )
      val nonCheckMove =
        Move.unsafeCreate(
          playingPiece,
          playingPiece.sourcePosition,
          Position.unsafeCreate(playingPiece.sourcePosition.file - 1, playingPiece.sourcePosition.rank - 1)
        )

      assert(Board.isKingInCheck(board, nonCheckMove) eqv Valid(false))

      val checkMove =
        Move.unsafeCreate(
          playingPiece,
          playingPiece.sourcePosition,
          Position.unsafeCreate(playingPiece.sourcePosition.file, playingPiece.sourcePosition.rank - 1)
        )

      assert(Board.isKingInCheck(board, checkMove) eqv Valid(true))
    }
  }

  checkAll("Eq[Board]", EqTests[Board].eqv)

}
