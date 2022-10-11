package chess.app

import cats.data.Validated.Valid
import cats.implicits._
import chess.FpFinalSpec
import chess.board.Board
import chess.pieces.{Pawn, Piece}
import chess.positions.Position
import org.scalacheck.{Arbitrary, Gen}

class MoveSpec extends FpFinalSpec {

  test("create a valid move") {
    forAll { (posFrom: Position, posTo: Position) =>
      val piece = Pawn(posFrom)
      val onePieceBoard = Board.unsafeCreate(Map(posFrom -> piece))

      assert(Move(onePieceBoard, posFrom, posTo) eqv Valid(Move.unsafeCreate(piece, posFrom, posTo)))
    }
  }

  test("create an invalid move due to the board not having 'From Position'") {
    forAll { (posFrom: Position, posTo: Position) =>
      val emptyBoard = Map.empty[Position, Piece]
      assert(Move(Board.unsafeCreate(emptyBoard), posFrom, posTo).isInvalid)
    }
  }

  test("create an invalid move due to the position") {
    implicit val intGen: Arbitrary[Int] = Arbitrary(Gen.choose(Int.MinValue, -1))

    forAll { (posFrom: Position, fileTo: Int, rankTo: Int) =>
      val oneBoardPiece = Map(posFrom -> Pawn(posFrom))
      val posTo = Position.unsafeCreate(fileTo, rankTo)
      assert(Move(Board.unsafeCreate(oneBoardPiece), posFrom, posTo).isInvalid)
    }
  }
}
