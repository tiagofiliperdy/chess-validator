package chess.positions

import cats.implicits._
import chess._
import chess.app.Configuration.IsValid
import chess.common.Validations

case class Position private(
  val file: File,
  val rank: Rank
) {

  def diff(pos: Position): (File, Rank) =
    (pos.file - file, pos.rank - rank)
}

object Position {
  import Validations._

  /**
    * Creates an instance of a Position without performing validations.
    * Should only be used in tests and/or App initiation state.
    */
  def unsafeCreate(
    file: File,
    rank: Rank
  ): Position = new Position(file, rank)

  def apply(
    file: File,
    rank: Rank
  ): IsValid[Position] =
    areCoordinatesInsideBoard(file, rank).map { case (f, r) => new Position(f, r) }

  def apply(line: String): IsValid[(Position, Position)] = {
    val filesAndRanks =
      (inputHas4Chars(line), inputHasCorrectFormat(line)).mapN((_, filesAndRanks) => filesAndRanks)

    filesAndRanks.andThen { fAndR =>
      (Position(fAndR._1, fAndR._2), Position(fAndR._3, fAndR._4)).mapN((p1, p2) => (p1, p2))
    }
  }
}
