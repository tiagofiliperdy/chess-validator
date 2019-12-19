package chess

import chess.pieces._
import chess.positions.Directions.Direction
import chess.positions.{Directions, Position}

import scala.collection.immutable.Map

object Board {
  val horizontal = ('a' to 'h')
  val vertical = (1 to 8).reverse
  val dimension = (0 to 7)

  def createBoard: Map[Position, Piece] = {
    val pWhiteStart = dimension.end
    val pWhitePawnStart = pWhiteStart - 1
    val pBlackStart = dimension.start
    val pBlackPawnStart = pBlackStart + 1

    val whitePieces = generatePieces(pWhiteStart, pWhitePawnStart).map(_.differentiatePlayer)
    val blackPieces = generatePieces(pBlackStart, pBlackPawnStart)

    val piecesMap: Map[Position, Piece] =
      (whitePieces ++ blackPieces).foldLeft(Map[Position, Piece]()) { (acc, next) =>
        acc + (next.sourcePosition -> next)
      }

    print(piecesMap)

    piecesMap
  }

  def updateBoard(
    board: Map[Position, Piece],
    piece: Piece,
    move: Move
  ): Map[Position, Piece] = (board - move.from) + ((move.to, piece))

  def isKingInCheck(
    board: Map[Position, Piece],
    color: Color
  ): Boolean = {
    val king: Option[(Position, Piece)] = board.find {
      case (_, k @ King(_, _)) => k.color.equals(color)
      case _                   => false
    }
    val knights: Set[(Position, Piece)] = board.filter {
      case (_, n @ Knight(_, _)) => !n.color.equals(color)
      case _                     => false
    }.toSet

    king.exists { k =>
      val kingPos = k._1
      val nearestPosPieces = Directions.all.flatMap { dir =>
        nearestPositionWithPiece(board, dir, kingPos.file + dir.shift._1, kingPos.rank + dir.shift._2, color)
      } ++ knights.map(_._1)

      nearestPosPieces.exists { pos =>
        board(pos).isValidMove(pos, kingPos, board)
      }
    }
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
  def nearestPositionWithPiece(
    board: Map[Position, Piece],
    dir: Direction,
    file: File,
    rank: Rank,
    color: Color
  ): Option[Position] =
    (positionExists(file, rank), board.get(Position(file, rank))) match {
      case (true, Some(p)) if !p.color.equals(color) =>
        Some(Position(file, rank))
      case (true, _) =>
        nearestPositionWithPiece(board, dir, file + dir.shift._1, rank + dir.shift._2, color)
      case _ =>
        None
    }

  def positionExists(
    file: File,
    rank: Rank
  ): Boolean = List(file, rank).forall(dimension.contains)

  private def generatePieces(
    start: Int,
    pawnStart: Int
  ): List[Piece] =
    List(
      Rook(Position(0, start)),
      Knight(Position(1, start)),
      Bishop(Position(2, start)),
      Queen(Position(3, start)),
      King(Position(4, start)),
      Bishop(Position(5, start)),
      Knight(Position(6, start)),
      Rook(Position(7, start))
    ) ++ dimension.map(file => Pawn(Position(file, pawnStart))).toList

  def print(board: Map[Position, Piece]): Unit = {
    val filesRow = horizontal.mkString("  | ", "|", " |")
    val separator = """--+--+-+-+-+-+-+-+--+--"""

    println()
    println(filesRow)
    println(separator)

    dimension.zip(vertical).foreach { rank =>
      val fullBoard =
        dimension
          .map { file =>
            board.get(Position(file, rank._1)) match {
              case Some(piece) => piece.identifier
              case _           => "."
            }
          }
          .mkString(rank._2 + " | ", " ", " | " + rank._2)

      println(fullBoard)
    }
    println(separator)
    println(filesRow)
    println()
  }

}
