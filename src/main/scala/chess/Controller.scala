package chess

import chess.pieces.{Color, Piece}
import chess.positions.Position
import com.whitehatgaming.UserInputFile

import scala.collection.immutable.Map
import scala.util.Try

class Controller(filePath: String) extends Output {

  def init(): Unit = {
    val result =
      for {
        uI <- Try(new UserInputFile(filePath)).toEither.left.map(_ => FileNotExistError(filePath).errorMsg)
        nextMove <- handleNextMove(uI, EmptyFileError(filePath))
        board = Board.createBoard
        _ = println(">>> Game Begins!")
        res <- loop(uI, nextMove, board, List(board))
      } yield res

    result match {
      case Left(e)  => println(e)
      case Right(_) => println(">>> Game Ended :D")
    }
  }

  def loop(
    uI: UserInputFile,
    moveCoordinates: List[Int],
    board: Map[Position, Piece],
    history: List[Map[Position, Piece]]
  ): Either[String, Unit] = {
    val move = Move(moveCoordinates)

    for {
      piece <- board.get(move.from).toRight(">>> No piece on starting move position!")
      dstInsideBoard = move.isToPositionInsideBoard
      pieceColor = piece.color
      res <- {
        if (dstInsideBoard && piece.isValidMove(move.from, move.to, board)) {
          val updatedBoard = Board.updateBoard(board, piece, move)
          // finds king of 'pieceColor' and validates if it is in check
          if (Board.isKingInCheck(updatedBoard, pieceColor)) {
            Left(s">>> Invalid move, ${move.toString}, it leaves your King in check.")
          } else {
            Board.print(updatedBoard)
            handleNextMove(uI, EOFError(filePath)).flatMap { next =>
              println(">>> Next Player")
              printCheckInfo(updatedBoard, piece.opponentColor)
              loop(uI, next, updatedBoard, updatedBoard :: history)
            }
          }
        } else Left(s">>> Invalid move, ${move.toString}")
      }
    } yield res
  }

  def printCheckInfo(
    board: Map[Position, Piece],
    color: Color
  ): Unit = if (Board.isKingInCheck(board, color)) println(">>> King is in Check!!!")

  def handleNextMove(
    uI: UserInputFile,
    error: InputFileError
  ): Either[String, List[Int]] =
    Try(uI.nextMove()).toEither.left.map(_ => error.errorMsg).flatMap { nextMoveRaw =>
      Option(nextMoveRaw).map(_.toList).toRight(error.errorMsg)
    }
}
