package chess.pieces

import chess.positions.Directions._
import chess.positions.Position

import scala.collection.immutable.HashMap

final case class Bishop(
  sourcePosition: Position,
  identifier: String = "b"
) extends Piece {
  override val directions: Set[Direction] = Set(NE, SE, NW, SW)

  override def isValidMove(
    from: Position,
    to: Position,
    board: HashMap[Position, Piece]
  ): Boolean = {
    val (fileDiff, rankDiff) = from.diff(to)

    (fileDiff, rankDiff) match {
      case (x, y) if x != 0 && y != 0 =>
        directions.map(_.shift).contains(fileDiff / Math.abs(fileDiff), rankDiff / Math.abs(rankDiff)) && super
          .isValidMove(
            from,
            to,
            board
          )
      case _ => false
    }
  }

  override def differentiatePlayer: Piece = Bishop(sourcePosition, identifier.toUpperCase())
}
