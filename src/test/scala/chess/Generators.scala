package chess

import chess.board.Board
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

  implicit def functionArb[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary {
      arbA.arbitrary.map(a => (_: A) => a)
    }

}
