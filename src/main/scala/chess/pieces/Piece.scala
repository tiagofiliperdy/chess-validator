package chess.pieces

import cats.Eq
import cats.data.Validated
import cats.implicits._
import chess.app.Configuration.IsValid
import chess.app.Move
import chess.board.Board
import chess.common.Color.{Black, White}
import chess.common.Color
import chess.positions.Directions.Direction
import chess.positions.Position
import chess.{File, Rank}

trait Piece {
  val directions: Set[Direction]

  def sourcePosition: Position
  def identifier: String
  def differentiatePlayer: Piece
  def color: Color = if (identifier.charAt(0).isUpper) White else Black

  /**
    * Validates moves between two positions, excluding start and end, when distance is greater than one.
    * Works for Rooks, Bishops and Queens.
    * @param move
    * @param board
    * @return
    */
  def isValidMove(
    move: Move,
    board: Board
  ): IsValid[Move] =
    (Piece.isNotTakingOwnPiece(move, board, color), Piece.isPathEmpty(move, board)).mapN((_, _) => move)

  def zipCoordinates(
    files: List[File],
    ranks: List[Rank]
  ): List[(File, Rank)] =
    files.zipAll(ranks, files.head, ranks.head)

}

object Piece {

  def isNotTakingOwnPiece(
    move: Move,
    board: Board,
    color: Color
  ): IsValid[Move] =
    Validated.condNec(
      !board.board.get(move.to).map(_.color).contains(color),
      move,
      "Move is not valid, To position contains piece of same color!"
    )

  def isPathEmpty(
    move: Move,
    board: Board
  ): IsValid[Move] =
    Validated.condNec(
      move.from.diff(move.to) match {
        case (x, y) if Math.abs(x) > 1 || Math.abs(y) > 1 =>
          val filesToCheck = inBetweenCoordinates(move.from.file, move.to.file)
          val ranksToCheck = inBetweenCoordinates(move.from.rank, move.to.rank)

          filesToCheck
            .zip(ranksToCheck)
            .traverse(pos => Position(pos._1, pos._2).map(p => !board.board.contains(p)))
            .map(_.forall(identity)) match {
            case Validated.Valid(value) => value
            case Validated.Invalid(_)   => false
          }
        case _ => true
      },
      move,
      "Move is not valid, the piece can't jump other pieces!"
    )

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
  private def inBetweenCoordinates(
    from: Int,
    to: Int
  ): List[Int] =
    (from == to, from < to) match {
      case (true, _) => List(from)
      case (_, true) => ((from + 1) until to).toList
      case (_, _)    => ((to + 1) until from).toList.reverse
    }

  implicit def eqPiece(implicit eqString: Eq[String]): Eq[Piece] =
    Eq.instance((p1, p2) => p1.identifier === p2.identifier)
}
