package chess.pieces

import cats.data.Validated
import cats.implicits._
import chess.Move
import chess.app.Configuration.IsValid
import chess.board.Board
import chess.positions.Directions.Direction
import chess.positions.{Directions, Position}

final case class King(
  sourcePosition: Position,
  identifier: String = "k"
) extends Piece {
  override val directions: Set[Direction] = Directions.all

  override def differentiatePlayer: Piece = King(sourcePosition, identifier.toUpperCase())

  override def isValidMoveV2(
    move: Move,
    board: Board
  ): IsValid[Move] = {
    val kingRule = {
      Validated.condNec(
        move.from.diff(move.to) match {
          case (0, y) => Math.abs(y) == 1
          case (x, 0) => Math.abs(x) == 1
          case (x, y) => Math.abs(x) == 1 && Math.abs(y) == 1
        },
        move,
        "King can't move more than 1 square position!"
      )
    }

    (kingRule, super.isValidMoveV2(move, board)).mapN((m, _) => m)
  }
}
