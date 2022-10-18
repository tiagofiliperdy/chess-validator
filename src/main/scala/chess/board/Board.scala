package chess.board

import cats._
import cats.data.Validated
import chess.app.Configuration.IsValid
import chess.app.Move
import chess.pieces._
import chess.positions.Directions.Direction
import chess.positions.{Directions, Position}
import chess.{File, Rank}

class Board private (val board: Map[Position, Piece]) {
  val horizontal: List[Char] = ('a' to 'h').toList
  val vertical: List[Int] = (1 to 8).reverse.toList

  def getPiece(position: Position): Option[Piece] = board.get(position)

}

object Board {
  import chess.common.Validations._

  val dimension: Range = (0 to 7)

  /**
    * Creates an instance of a Board without performing validations.
    * Should only be used in tests.
    */
  def unsafeCreate(board: Map[Position, Piece]): Board = new Board(board)

  /**
    * Should only be used as a beginning state for the App
    */
  def unsafeCreateGameBeginning: Board = {
    val pWhiteStart = dimension.end
    val pWhitePawnStart = pWhiteStart - 1
    val pBlackStart = dimension.start
    val pBlackPawnStart = pBlackStart + 1
    val whitePieces: Map[Position, Piece] =
      generatePieces(pWhiteStart, pWhitePawnStart).map { case (pos, piece) => pos -> piece.differentiatePlayer }
    val blackPieces: Map[Position, Piece] = generatePieces(pBlackStart, pBlackPawnStart)

    new Board(whitePieces ++ blackPieces)
  }

  def updateBoard(
    board: Board,
    move: Move
  ): Board =
    new Board((board.board - move.from) + ((move.to, move.piece)))

  private def generatePieces(
    start: Int,
    pawnStart: Int
  ): Map[Position, Piece] = {
    val p0 = Position.unsafeCreate(0, start)
    val p1 = Position.unsafeCreate(1, start)
    val p2 = Position.unsafeCreate(2, start)
    val p3 = Position.unsafeCreate(3, start)
    val p4 = Position.unsafeCreate(4, start)
    val p5 = Position.unsafeCreate(5, start)
    val p6 = Position.unsafeCreate(6, start)
    val p7 = Position.unsafeCreate(7, start)
    val pawns = dimension.toList.map(file => Position.unsafeCreate(file, pawnStart)).map(p => p -> Pawn(p)).toMap
    Map(
      p0 -> Rook(p0),
      p1 -> Knight(p1),
      p2 -> Bishop(p2),
      p3 -> Queen(p3),
      p4 -> King(p4),
      p5 -> Bishop(p5),
      p6 -> Knight(p6),
      p7 -> Rook(p7)
    ) ++ pawns
  }

  def isKingInCheck(
    board: Board,
    move: Move
  ): IsValid[Boolean] = {
    val king: Option[(Position, Piece)] = board.board.find {
      case (_, k @ King(_, _)) => k.color.equals(move.piece.color)
      case _                   => false
    }
    val knights: Set[(Position, Piece)] = board.board.filter {
      case (_, n @ Knight(_, _)) => !n.color.equals(move.piece.color)
      case _                     => false
    }.toSet

    val isKingInCheck =
      king.exists { k =>
        val kingPos = k._1
        val nearestPosPieces: List[Position] =
          Directions.all.toList.flatMap { dir =>
            nearestPositionWithPiece(
              board.board,
              dir,
              kingPos.file + dir.shift._1,
              kingPos.rank + dir.shift._2,
              move.piece.color
            ).toOption
          } ++ knights.map(_._1)

        nearestPosPieces.exists { pos =>
          Move(board, pos, kingPos).andThen(move => board.board(move.from).isValidMove(move, board)) match {
            case Validated.Valid(_)   => true
            case Validated.Invalid(_) => false
          }
        }
      }

    Validated.valid(isKingInCheck)
  }

  /**
    * Finds nearest positions with pieces of different color, excluding knights.
    * @param board
    * @param dir
    * @param file
    * @param rank
    * @param color
    * @return
    */
  private def nearestPositionWithPiece(
    board: Map[Position, Piece],
    dir: Direction,
    file: File,
    rank: Rank,
    color: Color
  ): IsValid[Position] =
    areCoordinatesInsideBoard(file, rank).andThen { case (f, r) => Position(f, r) }.andThen { pos =>
      if (board.contains(pos) && !board.get(pos).map(_.color).contains(color)) Validated.valid(pos)
      else nearestPositionWithPiece(board, dir, file + dir.shift._1, rank + dir.shift._2, color)
    }

  implicit val showBoard: Show[Board] =
    Show.show { b =>
      val filesRow = b.horizontal.mkString("  | ", "|", " |")
      val separator = """--+--+-+-+-+-+-+-+--+--"""
      val fullBoard: List[String] =
        dimension
          .zip(b.vertical)
          .map { rank =>
            dimension
              .map { file =>
                //TODO: get around the unsafeCreate here
                b.board.get(Position.unsafeCreate(file, rank._1)) match {
                  case Some(piece) => piece.identifier
                  case _           => "."
                }
              }
              .mkString(rank._2 + " | ", " ", " | " + rank._2)
          }
          .toList

      s"""
         |
         |$filesRow
         |$separator
         |${fullBoard.mkString("\n")}
         |$separator
         |$filesRow
         |
         |""".stripMargin
    }

  implicit def eqBoard(implicit eqMap: Eq[Map[Position, Piece]]): Eq[Board] = Eq.by(_.board)
}
