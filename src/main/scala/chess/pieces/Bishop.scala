package chess.pieces

import cats.data.Validated
import cats.implicits._
import chess.app.Configuration.IsValid
import chess.app.Move
import chess.board.Board
import chess.positions.Directions._
import chess.positions.Position

final case class Bishop(
  sourcePosition: Position,
  identifier: String = "b"
) extends Piece {
  override val directions: Set[Direction] = Set(NE, SE, NW, SW)

  override def isValidMove(
    move: Move,
    board: Board
  ): IsValid[Move] = {
    val (fileDiff, rankDiff) = move.from.diff(move.to)
    (fileDiff, rankDiff) match {
      case (x, y) if x != 0 && y != 0 =>
        val bishopRule =
          Validated.condNec(
            directions.map(_.shift).contains((fileDiff / Math.abs(fileDiff), rankDiff / Math.abs(rankDiff))),
            move,
            "Move is not valid for a Bishop!"
          )

        (bishopRule, super.isValidMove(move, board)).mapN((m, _) => m)
      case _ =>
        Validated.invalidNec("Move does not obey to Bishop basic rules of moving diagonally!")
    }

  }

  override def differentiatePlayer: Piece = Bishop(sourcePosition, identifier.toUpperCase())
}
