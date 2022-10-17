package chess.common

import cats.data._
import cats.implicits.catsSyntaxEq
import chess.{File, Rank}
import chess.app.Configuration.IsValid
import chess.board.Board
import chess.board.Board.dimension
import chess.pieces.Piece
import chess.positions.Position

object Validations {

  def inputHas4Chars(line: String): IsValid[String] =
    Validated.condNec(line.toCharArray.length == 4, line, "Users input does not have 4 characters!")

  def inputHasCorrectFormat(line: String): IsValid[(File, Rank, File, Rank)] =
    Validated.condNec(
      line.toCharArray.toList match {
        case f1 :: r1 :: f2 :: r2 :: Nil =>
          f1.isLetter && r1.isDigit && f2.isLetter && r2.isDigit
        case _ => false
      },
      line.toCharArray.toList match {
        case f1 :: r1 :: f2 :: r2 :: Nil =>
          (f1 - 97, 56 - r1, f2 - 97, 56 - r2)
      },
      "Users input does not respect the correct play format! i.e. 'h2h4'"
    )

  def areCoordinatesInsideBoard(
    file: File,
    rank: Rank
  ): IsValid[(File, Rank)] =
    Validated.condNec(
      List(file, rank).forall(dimension.contains),
      (file, rank),
      "Coordinates are not valid, they are out of board dimension!"
    )

  def isPositionInsideBoard(pos: Position): IsValid[Position] =
    Validated.condNec(
      List(pos.file, pos.rank).forall(dimension.contains),
      pos,
      "Position is not valid, it is out of board bounds!"
    )

  def isFromPositionAPiece(pos: Position, board: Board): IsValid[Piece] =
    Validated.condNec(
      board.getPiece(pos).isDefined,
      board.board(pos),
      "Position is not valid, it does not contain any piece!"
    )

  def isFromPositionDifferentThanTo(fromPos: Position, toPos: Position): IsValid[Unit] =
    Validated.condNec(
      fromPos =!= toPos,
      (),
      "From and To positions need to be different!"
    )

}
