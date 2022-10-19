package chess.app

import cats.Eq
import cats.implicits._
import chess.app.Configuration.IsValid
import chess.board.Board
import chess.pieces.Piece
import chess.positions.Position

case class Move private (
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
    to: Position,
    currentPlayer: Player
  ): IsValid[Move] =
    (
      isPositionInsideBoard(from),
      isPositionInsideBoard(to),
      isFromPositionAPiece(from, board),
      isFromPositionDifferentThanTo(from, to),
      fromPositionContainsPieceOfPlayerColor(from, board, currentPlayer)
    ).mapN((f, t, piece, _, _) => new Move(piece, f, t))

  implicit def eqMove(
    implicit eqPiece: Eq[Piece],
    eqPosition: Eq[Position]
  ): Eq[Move] =
    Eq.instance((m1, m2) => m1.piece === m2.piece && m1.from === m2.from && m1.to === m2.to)
}
