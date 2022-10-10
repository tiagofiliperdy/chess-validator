package chess

import cats.implicits._
import chess.app.Configuration.IsValid
import chess.positions.Position
import board.Board
import chess.pieces.Piece

case class Move private(
  val piece: Piece,
  val from: Position,
  val to: Position
)

object Move {
  import chess.common.Validations._

  /**
    * Creates an instance of a Move without performing validations.
    * Should only be used in tests.
    */
  def unsafeCreate(
    piece: Piece,
    from: Position,
    to: Position
  ): Move = new Move(piece, from, to)

  def apply(
    board: Board,
    from: Position,
    to: Position
  ): IsValid[Move] =
    (isPositionInsideBoard(from), isPositionInsideBoard(to), isFromPositionAPiece(from, board))
      .mapN((f, t, piece) => new Move(piece, f, t))
}
