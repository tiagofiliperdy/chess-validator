package chess.pieces

import cats.data.Validated
import cats.implicits._
import chess.app.Configuration.IsValid
import chess.board.Board
import chess.positions.Directions.Direction
import chess.positions.Position
import chess.File
import chess.app.Move

final case class Pawn(
  sourcePosition: Position,
  identifier: String = "p"
) extends Piece {
  // Not used
  override val directions: Set[Direction] = Set.empty

  val offset: File = if (identifier.charAt(0).isUpper) -1 else 1

  override def differentiatePlayer: Piece = Pawn(sourcePosition, identifier.toUpperCase())

  override def isValidMove(
    move: Move,
    board: Board
  ): IsValid[Move] = {
    val pawnRule =
      Validated.condNec(
        move.from.diff(move.to) match {
          case (0, y) if y == offset                                          => !board.board.contains(move.to)
          case (0, y) if Math.abs(y) == 2 && move.from.equals(sourcePosition) => !board.board.contains(move.to)
          case (x, y) if Math.abs(x) == 1 && Math.abs(y) == 1                 => board.board.contains(move.to)
          case _                                                              => false
        },
        move,
        "Pawn invalid move!"
      )

    (pawnRule, Piece.isNotTakingOwnPiece(move, board, color)).mapN((m, _) => m)
  }
}
