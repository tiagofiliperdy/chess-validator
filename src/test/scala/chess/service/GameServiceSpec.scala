package chess.service

import cats.implicits._
import chess.FpFinalSpec
import chess.app.Move
import chess.app.Player.{P1, P2}
import chess.board.Board
import chess.pieces.Piece
import chess.positions.Position
import chess.service.GameService.GameState

class GameServiceSpec extends FpFinalSpec {
  val service: LiveGameService#Service = new LiveGameService {}.gameService

  test("updates board of game state") {
    forAll { piece: Piece =>
      val toPosition = Position.unsafeCreate(piece.sourcePosition.file, piece.sourcePosition.rank + 1)
      val move = Move.unsafeCreate(piece, piece.sourcePosition, toPosition)
      val initialBoard = Board.unsafeCreate(Map(piece.sourcePosition -> piece, toPosition -> piece.differentiatePlayer))
      val initialGameState = GameState(initialBoard, List.empty, P1)

      val expectedBoardMap = (initialBoard.board - move.from) + ((move.to, piece))
      val expectedBoard = Board.unsafeCreate(expectedBoardMap)

      val result = service.updateBoard(move).run(initialGameState).value
      assert(result eqv (GameState(expectedBoard, List(expectedBoardMap), P1), expectedBoard))
    }
  }

  test("returns current state of game state") {
    assert(service.currentState.run(GameState.beginning).value eqv (GameState.beginning, GameState.beginning))
  }

  test("switch turns of game state") {
    assert(service.switchTurns().run(GameState.beginning).value eqv (GameState.beginning.copy(player = P2), P2))
  }

}
