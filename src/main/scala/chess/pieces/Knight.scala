package chess.pieces

import cats.data.Validated
import cats.implicits._
import chess.app.Configuration.IsValid
import chess.app.Move
import chess.board.Board
import chess.positions.Directions.Direction
import chess.positions.Position

final case class Knight(
  sourcePosition: Position,
  identifier: String = "n"
) extends Piece {
  // Not used
  override val directions: Set[Direction] = Set.empty

  override def differentiatePlayer: Piece = Knight(sourcePosition, identifier.toUpperCase)

  override def isValidMove(
    move: Move,
    board: Board
  ): IsValid[Move] = {
    val diff = move.from.diff(move.to)
    val knightRule =
      Validated.condNec(
        (Math.abs(diff._1), Math.abs(diff._2)) match {
          case (1, 2) => true
          case (2, 1) => true
          case _      => false
        },
        move,
        "Knight has to move in an L shape!"
      )

    (knightRule, super.isValidMove(move, board)).mapN((m, _) => m)
  }
}
