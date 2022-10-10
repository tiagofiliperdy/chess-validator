package chess.pieces

import cats.data.Validated
import cats.implicits._
import chess.Move
import chess.app.Configuration.IsValid
import chess.board.Board
import chess.positions.Directions._
import chess.positions.Position

final case class Rook(
  sourcePosition: Position,
  identifier: String = "r"
) extends Piece {
  override val directions: Set[Direction] = Set(N, S, E, W)

  override def differentiatePlayer: Piece = Rook(sourcePosition, identifier.toUpperCase)

  override def isValidMoveV2(
    move: Move,
    board: Board
  ): IsValid[Move] = {
    val rookRule =
      Validated.condNec(
        move.from.diff(move.to) match {
          case (0, y) if y != 0 => true
          case (x, 0) if x != 0 => true
          case _                => false
        },
        move,
        "Rook invalid move!"
      )

    (rookRule, super.isValidMoveV2(move, board)).mapN((m, _) => m)
  }
}
