package chess

import chess.app.Move
import chess.pieces.Piece
import chess.positions.Position

trait PiecesProperties {

  val rightMove = (piece: Piece) =>
    Move.unsafeCreate(
      piece,
      piece.sourcePosition,
      Position.unsafeCreate(piece.sourcePosition.file + 2, piece.sourcePosition.rank)
    )

  val leftMove = (piece: Piece) =>
    Move.unsafeCreate(
      piece,
      piece.sourcePosition,
      Position.unsafeCreate(piece.sourcePosition.file - 2, piece.sourcePosition.rank)
    )

  val upwards = (piece: Piece) =>
    Move.unsafeCreate(
      piece,
      piece.sourcePosition,
      Position.unsafeCreate(piece.sourcePosition.file, piece.sourcePosition.rank + 2)
    )

  val downwards = (piece: Piece) =>
    Move.unsafeCreate(
      piece,
      piece.sourcePosition,
      Position.unsafeCreate(piece.sourcePosition.file, piece.sourcePosition.rank - 2)
    )

  val diagonalPositiveMove = (piece: Piece) =>
    Move.unsafeCreate(
      piece,
      piece.sourcePosition,
      Position.unsafeCreate(piece.sourcePosition.file + 2, piece.sourcePosition.rank + 2)
    )

  val diagonalNegativeMove = (piece: Piece) =>
    Move.unsafeCreate(
      piece,
      piece.sourcePosition,
      Position.unsafeCreate(piece.sourcePosition.file - 1, piece.sourcePosition.rank - 1)
    )

  val unorthodoxMove1 = (piece: Piece) =>
    Move.unsafeCreate(
      piece,
      piece.sourcePosition,
      Position.unsafeCreate(piece.sourcePosition.file + 1, piece.sourcePosition.rank + 2)
    )

  val unorthodoxMove2 = (piece: Piece) =>
    Move.unsafeCreate(
      piece,
      piece.sourcePosition,
      Position.unsafeCreate(piece.sourcePosition.file - 1, piece.sourcePosition.rank + 2)
    )

  val unorthodoxMove3 = (piece: Piece) =>
    Move.unsafeCreate(
      piece,
      piece.sourcePosition,
      Position.unsafeCreate(piece.sourcePosition.file - 1, piece.sourcePosition.rank - 2)
    )

}
