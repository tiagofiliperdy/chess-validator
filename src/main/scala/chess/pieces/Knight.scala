package chess.pieces

import chess.positions.Directions.Direction
import chess.positions.Position

import scala.collection.immutable.HashMap

final case class Knight(
  sourcePosition: Position,
  identifier: String = "n"
) extends Piece {
  // Not used
  override val directions: Set[Direction] = Set.empty

  override def differentiatePlayer: Piece = Knight(sourcePosition, identifier.toUpperCase)

  override def isValidMove(
    from: Position,
    to: Position,
    board: HashMap[Position, Piece]
  ): Boolean = {
    val isTakingOwnPiece = board.get(to).map(_.color).contains(color)
    val diff = from.diff(to)

    (Math.abs(diff._1), Math.abs(diff._2)) match {
      case (1, 2) => !isTakingOwnPiece
      case (2, 1) => !isTakingOwnPiece
      case _      => false
    }
  }
}
