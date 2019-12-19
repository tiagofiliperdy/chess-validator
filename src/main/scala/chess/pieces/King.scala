package chess.pieces

import chess.positions.Directions.Direction
import chess.positions.{Directions, Position}

import scala.collection.immutable.Map

final case class King(
  sourcePosition: Position,
  identifier: String = "k"
) extends Piece {
  override val directions: Set[Direction] = Directions.all

  override def differentiatePlayer: Piece = King(sourcePosition, identifier.toUpperCase())

  override def isValidMove(
    from: Position,
    to: Position,
    board: Map[Position, Piece]
  ): Boolean = {
    val isTakingOwnPiece = board.get(to).map(_.color).contains(color)

    from.diff(to) match {
      case (0, y) => Math.abs(y) == 1 && !isTakingOwnPiece
      case (x, 0) => Math.abs(x) == 1 && !isTakingOwnPiece
      case (x, y) => Math.abs(x) == 1 && Math.abs(y) == 1 && !isTakingOwnPiece
    }
  }
}
