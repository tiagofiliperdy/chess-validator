package chess.app

import cats.data.Validated.Valid
import cats.implicits._
import cats.kernel.laws.discipline.EqTests
import chess.FpFinalSpec
import chess.app.Player.{P1, P2}
import chess.board.Board
import chess.pieces.{Pawn, Piece}
import chess.positions.Position
import org.scalacheck.{Arbitrary, Gen}

class MoveSpec extends FpFinalSpec {

  test("create a valid move") {
    forAll { (posFrom: Position, posTo: Position) =>
      val piece = Pawn(posFrom)
      val onePieceBoard = Board.unsafeCreate(Map(posFrom -> piece))

      assert(Move(onePieceBoard, posFrom, posTo, P2) eqv Valid(Move.unsafeCreate(piece, posFrom, posTo)))
    }
  }

  test("create an invalid move due to the board not having 'From Position'") {
    forAll { (posFrom: Position, posTo: Position) =>
      val emptyBoard = Map.empty[Position, Piece]
      assert(Move(Board.unsafeCreate(emptyBoard), posFrom, posTo, P2).isInvalid)
    }
  }

  test("create an invalid move due to the 'From Position' not having piece of same color") {
    forAll { (posFrom: Position, posTo: Position) =>
      val piece = Pawn(posFrom)
      val onePieceBoard = Board.unsafeCreate(Map(posFrom -> piece))
      assert(Move(Board.unsafeCreate(onePieceBoard.board), posFrom, posTo, P1).isInvalid)
    }
  }

  test("create an invalid move due to the position") {
    implicit val intGen: Arbitrary[Int] = Arbitrary(Gen.choose(Int.MinValue, -1))

    forAll { (posFrom: Position, fileTo: Int, rankTo: Int) =>
      val oneBoardPiece = Map(posFrom -> Pawn(posFrom))
      val posTo = Position.unsafeCreate(fileTo, rankTo)
      assert(Move(Board.unsafeCreate(oneBoardPiece), posFrom, posTo, P2).isInvalid)
    }
  }

  checkAll("Eq[Move]", EqTests[Move].eqv)
}
