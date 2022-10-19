package chess.common

import cats.data.Validated
import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import chess.app.Configuration.IsValid
import chess.app.Player.{P1, P2}
import chess.board.Board
import chess.pieces.{Bishop, Piece}
import chess.positions.Position
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValidationsSpec extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  import Validations._

  implicit val ranks: Arbitrary[Int] = Arbitrary(Gen.choose(1, 8))
  implicit val files: Arbitrary[Char] = Arbitrary(Gen.choose('a', 'h'))
  implicit val positionArb: Arbitrary[Position] = Arbitrary {
    for {
      file <- Gen.choose(Board.dimension.start, Board.dimension.end)
      rank <- Gen.choose(Board.dimension.start, Board.dimension.end)
    } yield Position.unsafeCreate(file, rank)
  }
  implicit def bishopArb(implicit posArb: Arbitrary[Position]): Arbitrary[Piece] = Arbitrary {
    for {
      pos <- posArb.arbitrary
    } yield Bishop(pos)
  }

  test("valid input has 4 chars") {
    implicit val g: Arbitrary[String] =
      Arbitrary(Gen.listOfN(4, Gen.alphaChar).map(_.mkString("")))

    forAll { input: String =>
      assert(inputHas4Chars(input) eqv Valid(input))
    }
  }

  test("invalid input has 4 chars") {
    implicit val invalidInput: Arbitrary[String] = Arbitrary {
      for {
        size <- Gen.choose(1, 32).withFilter(_ != 4)
        input <- Gen.listOfN(size, Gen.alphaChar).map(_.mkString(""))
      } yield input
    }

    forAll { input: String =>
      assert(inputHas4Chars(input).isInvalid)
    }
  }

  test("valid input has correct format") {
    forAll { (rankF: Char, fileF: Int, rankT: Char, fileT: Int) =>
      val asStr = s"$rankF$fileF$rankT$fileT"
      val translatedValues = (asStr.charAt(0) - 97, 56 - asStr.charAt(1), asStr.charAt(2) - 97, 56 - asStr.charAt(3))
      assert(inputHasCorrectFormat(asStr) eqv Valid(translatedValues))
    }
  }

  test("invalid input has correct format") {
    forAll { (rankF: Int, fileF: Char, rankT: Int, fileT: Char) =>
      val asStr = s"$rankF$fileF$rankT$fileT"
      assert(inputHasCorrectFormat(asStr).isInvalid)
    }
  }

  test("valid are coordinates inside board") {
    implicit val ranks: Arbitrary[Int] = Arbitrary(Gen.choose(0, 7))

    forAll { (rank: Int, file: Int) =>
      assert(areCoordinatesInsideBoard(file, rank) eqv Valid((file, rank)))
    }
  }

  test("invalid are coordinates inside board") {
    implicit val ranks: Arbitrary[Int] = Arbitrary(Gen.choose(7, 16))

    forAll { (rank: Int, file: Int) =>
      assert(areCoordinatesInsideBoard(file, rank).isInvalid)
    }
  }

  test("valid is position inside board") {
    forAll { (pos: Position) =>
      assert(isPositionInsideBoard(pos) eqv Valid(pos))
    }
  }

  test("invalid is position inside board") {
    implicit val positionArb: Arbitrary[Position] = Arbitrary {
      for {
        file <- Gen.choose(Board.dimension.end + 1, Int.MaxValue)
        rank <- Gen.choose(Int.MinValue, Board.dimension.start - 1)
      } yield Position.unsafeCreate(file, rank)
    }

    forAll { (pos: Position) =>
      assert(isPositionInsideBoard(pos).isInvalid)
    }
  }

  test("valid is from position a piece") {
    forAll { (piece: Piece) =>
      val board = Board.unsafeCreate(Map(piece.sourcePosition -> piece))
      assert(isFromPositionAPiece(piece.sourcePosition, board) eqv Valid(piece))
    }
  }

  test("invalid is from position a piece") {
    forAll { (piece: Piece) =>
      val board = Board.unsafeCreate(Map(Position.unsafeCreate(32, 33) -> piece))
      assert(isFromPositionAPiece(piece.sourcePosition, board).isInvalid)
    }
  }

  test("validate is from position different than to position") {
    forAll { (posF: Position, posT: Position) =>
      val res: IsValid[Unit] =
        if (posF =!= posT) Valid(())
        else Validated.invalidNec("From and To positions need to be different!")
      assert(isFromPositionDifferentThanTo(posF, posT) eqv res)
    }
  }

  test("valid from position contains piece of player color") {
    forAll { (piece: Piece) =>
      val board = Board.unsafeCreate(Map(piece.sourcePosition -> piece))
      assert(fromPositionContainsPieceOfPlayerColor(piece.sourcePosition, board, P2) eqv Valid(()))
    }
  }

  test("invalid from position contains piece of player color") {
    forAll { (piece: Piece) =>
      val board = Board.unsafeCreate(Map(piece.sourcePosition -> piece))
      assert(fromPositionContainsPieceOfPlayerColor(piece.sourcePosition, board, P1).isInvalid)
    }
  }

}
