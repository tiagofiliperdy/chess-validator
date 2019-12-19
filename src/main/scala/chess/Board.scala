package chess

import chess.pieces._
import chess.positions.Position

import scala.collection.immutable.HashMap

object Board {
  val horizontal = ('a' to 'h')
  val vertical = (1 to 8).reverse
  val dimension = (0 to 7)

  def createBoard: HashMap[Position, Piece] = {
    val pWhiteStart = dimension.end
    val pWhitePawnStart = pWhiteStart - 1
    val pBlackStart = dimension.start
    val pBlackPawnStart = pBlackStart + 1

    val whitePieces = generatePieces(pWhiteStart, pWhitePawnStart).map(_.differentiatePlayer)
    val blackPieces = generatePieces(pBlackStart, pBlackPawnStart)

    val piecesMap: HashMap[Position, Piece] =
      (whitePieces ++ blackPieces).foldLeft(HashMap[Position, Piece]()) { (acc, next) =>
        acc + (next.sourcePosition -> next)
      }

    print(piecesMap)

    piecesMap
  }

  def updateBoard(
    board: HashMap[Position, Piece],
    piece: Piece,
    move: Move
  ): HashMap[Position, Piece] = (board - move.from) + ((move.to, piece))

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

  def print(board: HashMap[Position, Piece]): Unit = {
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
