package chess.pieces

import chess.positions.Directions.Direction
import chess.positions.{Directions, Position}

import scala.collection.immutable.Map

final case class Queen(
  sourcePosition: Position,
  identifier: String = "q"
) extends Piece {
  override val directions: Set[Direction] = Directions.all

  override def isValidMove(
    from: Position,
    to: Position,
    board: Map[Position, Piece]
  ): Boolean = {
    val (fileDiff, rankDiff) = from.diff(to)

    (fileDiff, rankDiff) match {
      case (0, y) if y != 0 => super.isValidMove(from, to, board)
      case (x, 0) if x != 0 => super.isValidMove(from, to, board)
      case _ =>
        directions.map(_.shift).contains(fileDiff / Math.abs(fileDiff), rankDiff / Math.abs(rankDiff)) && super
          .isValidMove(from, to, board)
    }
  }

  override def differentiatePlayer: Piece = Queen(sourcePosition, identifier.toUpperCase())
}
