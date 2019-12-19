package chess

import java.io.FileNotFoundException

import chess.pieces.Piece
import chess.positions.Position
import com.whitehatgaming.UserInputFile

import scala.collection.immutable.HashMap
import scala.io.StdIn
import scala.util.Try

class Controller(filePath: String) extends Output {

  def init(): Unit = {
    val result =
      for {
        uI <- Try(new UserInputFile(filePath)).toEither
        nextMove <- Option(uI.nextMove())
          .map(_.toList)
          .toRight(new InterruptedException(s"File with path '$filePath' is empty."))
        board = Board.createBoard
        _ = println(">>> Game Begins!")
      } yield loop(uI, nextMove, board, List(board))

    result match {
      case Left(_: FileNotFoundException) => println(s"File with path '$filePath' doesn't exist.")
      case Left(e: InterruptedException)  => println(e.getMessage)
      case Right(_)                       => println(">>> Game Ended :D")
    }
  }

  def loop(
    uI: UserInputFile,
    moveCoordinates: List[Int],
    board: HashMap[Position, Piece],
    history: List[HashMap[Position, Piece]]
  ): Unit = {
    val move = Move(moveCoordinates)

    val result =
      for {
        piece <- board.get(move.from).toRight(">>> No piece on starting move position!")
        dstInsideBoard = move.isToPositionInsideBoard
        res <- {
          if (dstInsideBoard && piece.isValidMove(move.from, move.to, board)) {
            val updatedBoard = Board.updateBoard(board, piece, move)
            Board.print(updatedBoard)
            Try(uI.nextMove()).toEither
              .map { nextMove =>
                println(">>> Next Player")
                loop(uI, nextMove.toList, updatedBoard, updatedBoard :: history)
              }
              .left
              .map(_ => s">>> File with path '$filePath' has reached EOF.")
          } else Left(s">>> Invalid move, ${move.toString}")
        }
      } yield res

    result match {
      case Left(e) => handleError(e)
      case _       => ()
    }
  }

  def handleError(message: String): Unit = {
    println(message)
    println(">>> Press Any to exit the game<<<")
    StdIn.readLine()
    ()
  }
}
