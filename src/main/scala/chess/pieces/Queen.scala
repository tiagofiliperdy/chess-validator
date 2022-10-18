package chess.pieces

import cats.data.Validated
import cats.implicits._
import chess.app.Configuration.IsValid
import chess.app.Move
import chess.board.Board
import chess.positions.Directions.Direction
import chess.positions.{Directions, Position}

final case class Queen(
  sourcePosition: Position,
  identifier: String = "q"
) extends Piece {
  override val directions: Set[Direction] = Directions.all

  override def isValidMove(
    move: Move,
    board: Board
  ): IsValid[Move] = {
    val (fileDiff, rankDiff) = move.from.diff(move.to)

    val queenRule =
      Validated.condNec(
        (fileDiff, rankDiff) match {
          case (0, y) if y != 0 => true
          case (x, 0) if x != 0 => true
          case (x, y) if Math.abs(x) == Math.abs(y) =>
            directions.map(_.shift).contains(fileDiff / Math.abs(fileDiff), rankDiff / Math.abs(rankDiff))
          case _ => false
        },
        move,
        "Queen invalid move!"
      )

    (queenRule, super.isValidMove(move, board)).mapN((m, _) => m)
  }

  override def differentiatePlayer: Piece = Queen(sourcePosition, identifier.toUpperCase())
}
