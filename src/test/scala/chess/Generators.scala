package chess

import chess.app.Move
import chess.board.Board
import chess.pieces.Pawn
import chess.positions.Position
import org.scalacheck.{Arbitrary, Gen}

trait Generators {

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
  implicit val moveArb: Arbitrary[Move] = Arbitrary {
    for {
      fromPos <- positionArb.arbitrary
      toPos <- positionArb.arbitrary
    } yield Move.unsafeCreate(Pawn(fromPos), fromPos, toPos)
  }

  implicit def functionArb[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary {
      arbA.arbitrary.map(a => (_: A) => a)
    }

}
