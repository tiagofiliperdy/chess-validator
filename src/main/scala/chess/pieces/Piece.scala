package chess.pieces

import chess.{File, Rank}
import chess.positions.Directions.Direction
import chess.positions.Position

import scala.collection.immutable.HashMap

trait Piece {
  val directions: Set[Direction]

  def sourcePosition: Position
  def identifier: String
  def differentiatePlayer: Piece
  def color: Color = if (identifier.charAt(0).isUpper) White else Black

  /**
    * Validates moves between two positions, excluding start and end, when distance is greater than one.
    * Works for Rooks, Bishops and Queens.
    * @param from source position.
    * @param to destination position.
    * @param board current board state.
    * @return Boolean.
    */
  def isValidMove(
    from: Position,
    to: Position,
    board: HashMap[Position, Piece]
  ): Boolean = {
    val isTakingOwnPiece = board.get(to).map(_.color).contains(color)
    val isPathEmpty =
      from.diff(to) match {
        case (x, y) if Math.abs(x) > 1 && Math.abs(y) > 1 =>
          val filesToCheck = inBetweenCoordinates(from.file, to.file)
          val ranksToCheck = inBetweenCoordinates(from.rank, to.rank)

          zipCoordinates(filesToCheck, ranksToCheck).forall(pos => board.get(Position(pos._1, pos._2)).isEmpty)
        case _ => true
      }

    isPathEmpty && !isTakingOwnPiece
  }

  def zipCoordinates(
    files: List[File],
    ranks: List[Rank]
  ): List[(File, Rank)] =
    files.zipAll(ranks, files.head, ranks.head)

  /**
    * Generates board numbers representing Files/Ranks to check.
    * Example:
    * chess.Move of a Bishop from (7,1) to (1, 7)
    * For file input is from: 7, to: 1 and it will generate List(6,5,4,3,2)
    * For rank input is from: 1, to: 7 and it will generate List(2,3,4,5,6)
    * @param from
    * @param to
    * @return
    */
  def inBetweenCoordinates(
    from: Int,
    to: Int
  ): List[Int] =
    (from == to, from < to) match {
      case (true, _) => List(from)
      case (_, true) => ((from + 1) until to).toList
      case (_, _)    => ((to + 1) until from).toList.reverse
    }

}
