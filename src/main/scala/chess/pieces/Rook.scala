package chess.pieces

import chess.positions.Directions._
import chess.positions.Position

import scala.collection.immutable.Map

final case class Rook(
  sourcePosition: Position,
  identifier: String = "r"
) extends Piece {
  override val directions: Set[Direction] = Set(N, S, E, W)

  override def differentiatePlayer: Piece = Rook(sourcePosition, identifier.toUpperCase)

  override def isValidMove(
    from: Position,
    to: Position,
    board: Map[Position, Piece]
  ): Boolean =
    from.diff(to) match {
      case (0, y) if y != 0 => super.isValidMove(from, to, board)
      case (x, 0) if x != 0 => super.isValidMove(from, to, board)
      case _                => false
    }
}
