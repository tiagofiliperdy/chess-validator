package chess.board

import cats.Show
import cats.data.State
import cats.implicits._
import chess.Move
import chess.board.BoardService.{BoardOp, BoardState}
import chess.pieces.Piece
import chess.positions.Position

trait BoardService {
  import BoardService._

  val boardService: Service

  trait Service {

    def updateBoard(move: Move): BoardOp[Board]

    def currentState: BoardOp[BoardState]
  }
}

object BoardService {
  type BoardOp[A] = State[BoardState, A]

  /**
    * Represents a state containing the current board and its history.
    */
  case class BoardState(
    board: Board,
    history: List[Map[Position, Piece]]
  ) {

    def updateBoard(move: Move): (BoardState, Board) = {
      val updatedBoard: Board = Board.updateBoard(board, move)
      (copy(board = updatedBoard, updatedBoard.board :: history), updatedBoard)
    }
  }

  object BoardState {
    def beginning: BoardState = BoardState(Board.unsafeCreateGameBeginning, Nil)

    implicit val boardStateShow: Show[BoardState] = Show.show(bs => bs.board.show)
  }
}

trait LiveBoardService extends BoardService {
  override val boardService: Service = new Service {
    override def updateBoard(move: Move): BoardOp[Board] = State(_.updateBoard(move))

    override def currentState: BoardOp[BoardState] = State.get[BoardState]
  }
}
