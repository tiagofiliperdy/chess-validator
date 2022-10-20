package chess

import cats.data.{NonEmptyChain, State, Validated, ValidatedNec}
import cats.effect.IO
import cats.implicits._
import chess.app.Configuration.IsValid
import chess.app.Player.{P1, P2}
import chess.app.{Move, Player}
import chess.board.Board
import chess.pieces.{Bishop, King, Knight, Pawn, Piece, Queen, Rook}
import chess.positions.Position
import chess.service.GameService.{GameOp, GameState}
import org.scalacheck.{Arbitrary, Gen}

trait Generators {

  implicit def ioArb[A](implicit aArb: Arbitrary[A]): Arbitrary[IO[A]] =
    Arbitrary(aArb.arbitrary.map(IO(_)))

  /**
    * Only creates valid Position even though file and rank are not validated
    */
  implicit val positionArb: Arbitrary[Position] = Arbitrary {
    for {
      file <- Gen.choose(Board.dimension.start, Board.dimension.end)
      rank <- Gen.choose(Board.dimension.start, Board.dimension.end)
    } yield Position.unsafeCreate(file, rank)
  }

  /**
    * This Move does not have the From position validated due to lack of a Board
    */
  implicit val pawnMoveArb: Arbitrary[Move] = Arbitrary {
    for {
      fromPos <- positionArb.arbitrary
      toPos <- positionArb.arbitrary if toPos != fromPos
    } yield Move.unsafeCreate(Pawn(fromPos), fromPos, toPos)
  }

  implicit val arbPlayer: Arbitrary[Player] = Arbitrary(Gen.oneOf(P1, P2))

  implicit def boardArb(implicit pieceArb: Arbitrary[Piece]): Arbitrary[Board] = Arbitrary {
    Gen.mapOf[Position, Piece](pieceArb.arbitrary.map(p => (p.sourcePosition, p))).map(Board.unsafeCreate)
  }

  implicit def arbPiece(implicit arbPos: Arbitrary[Position]): Arbitrary[Piece] = Arbitrary {
    for {
      pos1 <- arbPos.arbitrary
      pos2 <- arbPos.arbitrary
      pos3 <- arbPos.arbitrary
      pos4 <- arbPos.arbitrary
      pieces = List(Pawn(pos1), Bishop(pos2), Queen(pos3), Rook(pos4))
      piece <- Gen.oneOf(pieces)
    } yield piece
  }

  implicit def arbBishop(implicit arbPos: Arbitrary[Position]): Arbitrary[Bishop] = Arbitrary {
    for {
      pos <- arbPos.arbitrary
    } yield Bishop(pos)
  }

  implicit def arbQueen(implicit arbPos: Arbitrary[Position]): Arbitrary[Queen] = Arbitrary {
    for {
      pos <- arbPos.arbitrary
    } yield Queen(pos)
  }

  implicit def arbRook(implicit arbPos: Arbitrary[Position]): Arbitrary[Rook] = Arbitrary {
    for {
      pos <- arbPos.arbitrary
    } yield Rook(pos)
  }

  implicit def arbKing(implicit arbPos: Arbitrary[Position]): Arbitrary[King] = Arbitrary {
    for {
      pos <- arbPos.arbitrary
    } yield King(pos)
  }

  implicit def arbPawn(implicit arbPos: Arbitrary[Position]): Arbitrary[Pawn] = Arbitrary {
    for {
      pos <- arbPos.arbitrary
    } yield Pawn(pos)
  }

  implicit def arbKnight(implicit arbPos: Arbitrary[Position]): Arbitrary[Knight] = Arbitrary {
    for {
      pos <- arbPos.arbitrary
    } yield Knight(pos)
  }

  implicit def functionArb[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary {
      arbA.arbitrary.map(a => (_: A) => a)
    }

  implicit def isValidArb[A](implicit arbA: Arbitrary[A]): Arbitrary[IsValid[A]] = {
    val validGen: Gen[IsValid[A]] = arbA.arbitrary.map(_.validNec[String])
    val invalidGen: Gen[IsValid[A]] =
      Gen.nonEmptyListOf(Arbitrary.arbitrary[String]).map(xs => NonEmptyChain.fromSeq(xs).get.invalid[A])

    Arbitrary(Gen.oneOf(validGen, invalidGen))
  }

  implicit def gameOpArb[A](
    implicit arbA: Arbitrary[A],
    arbBoard: Arbitrary[Board],
    arbPlayer: Arbitrary[Player]
  ): Arbitrary[GameOp[A]] = Arbitrary {
    for {
      a <- arbA.arbitrary
      board <- arbBoard.arbitrary
      player <- arbPlayer.arbitrary
    } yield State((_: GameState) => (GameState(board, List(board.board), player), a))
  }

}
