package chess.app

import cats.kernel.laws.discipline.EqTests
import chess.FpFinalSpec
import chess.service.GameService.GameState

class GameStateSpec extends FpFinalSpec {
  checkAll("Eq[GameState]", EqTests[GameState].eqv)
}
