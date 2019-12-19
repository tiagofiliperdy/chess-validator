package chess

import chess.positions.Position

final case class Move(
  from: Position,
  to: Position
) {

  def isToPositionInsideBoard: Boolean = List(to.file, to.rank).forall(Board.dimension.contains)

  override def toString: String =
    s"from: ${(from.file + 97).toChar}${(56 - from.rank).toChar}, to: ${(to.file + 97).toChar}${(56 - to.rank).toChar}"
}

object Move {

  def apply(move: List[Int]): Move = Move(Position(move.head, move(1)), Position(move(2), move(3)))

}
