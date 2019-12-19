package chess.pieces

import chess.File
import chess.positions.Directions.Direction
import chess.positions.Position

import scala.collection.immutable.Map

final case class Pawn(
  sourcePosition: Position,
  identifier: String = "p"
) extends Piece {
  // Not used
  override val directions: Set[Direction] = Set.empty

  val offset: File = if (identifier.charAt(0).isUpper) -1 else 1

  override def differentiatePlayer: Piece = Pawn(sourcePosition, identifier.toUpperCase())

  override def isValidMove(
    from: Position,
    to: Position,
    board: Map[Position, Piece]
  ): Boolean = {
    val isTakingOwnPiece = board.get(to).map(_.color).contains(color)

    from.diff(to) match {
      case (0, y) if y == offset                                     => board.get(to).isEmpty && !isTakingOwnPiece
      case (0, y) if Math.abs(y) == 2 && from.equals(sourcePosition) => board.get(to).isEmpty && !isTakingOwnPiece
      case (x, y) if Math.abs(x) == 1 && Math.abs(y) == 1            => board.get(to).isDefined && !isTakingOwnPiece
      case _                                                         => false
    }
  }
}
