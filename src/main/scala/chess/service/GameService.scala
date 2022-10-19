package chess.service

import cats.Show
import cats.data.State
import cats.implicits._
import chess.app.Player.{P1, P2}
import chess.app.{Move, Player}
import chess.board.Board
import chess.pieces.Piece
import chess.positions.Position
import chess.service.GameService.{GameOp, GameState}

trait GameService {
  import GameService._

  val gameService: Service

  trait Service {

    def updateBoard(move: Move): GameOp[Board]

    def currentState: GameOp[GameState]

    def switchTurns(): GameOp[Player]
  }
}

object GameService {
  type GameOp[A] = State[GameState, A]

  /**
    * Represents a state containing the current board and its history.
    */
  case class GameState(
    board: Board,
    history: List[Map[Position, Piece]],
    player: Player
  ) {

    def updateBoard(move: Move): (GameState, Board) = {
      val updatedBoard: Board = Board.updateBoard(board, move)
      (copy(board = updatedBoard, updatedBoard.board :: history), updatedBoard)
    }

    def switchTurns: (GameState, Player) = {
      val nextPlayer = if(player == P1) P2 else P1
      (copy(player = nextPlayer), nextPlayer)
    }
  }

  object GameState {
    def beginning: GameState = GameState(Board.unsafeCreateGameBeginning, Nil, P1)

    implicit val boardStateShow: Show[GameState] = Show.show(bs => bs.board.show)
  }
}

trait LiveGameService extends GameService {
  override val gameService: Service = new Service {
    override def updateBoard(move: Move): GameOp[Board] = State(_.updateBoard(move))

    override def currentState: GameOp[GameState] = State.get[GameState]

    override def switchTurns(): GameOp[Player] = State(_.switchTurns)
  }
}
