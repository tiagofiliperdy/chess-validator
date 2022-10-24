package chess.app

import cats.effect.unsafe.implicits.global
import chess.fakes.FakeConsole
import chess.service.GameService.GameState
import chess.service.LiveGameService
import org.scalatest.funsuite.AnyFunSuite

class AppSpec extends AnyFunSuite {

  test("No errors play flow") {
    val fakeEnv = new LiveGameService with FakeConsole {
      override var linesToRead: List[String] = List(
        "h2h4",
        "d7d5",
        "b1c3",
        "a7a5",
        "END"
      )
    }

    val res = App.run().run(fakeEnv).run(GameState.beginning).value.unsafeRunSync()

    assert(res.isRight)
  }

  test("Play flow with invalid king check move") {
    val fakeEnv = new LiveGameService with FakeConsole {
      override var linesToRead: List[String] = List(
        "h2h4",
        "d7d5",
        "b1c3",
        "a7a5",
        "e2e4",
        "c7c6",
        "c3d5",
        "c6d5",
        "e4d5",
        "d8d5",
        "f1e2",
        "g8f6",
        "g1h3",
        "d5g2",
        "h3g5",
        "g2h1", // first check with queen from player2
        "e2d3", // Move is invalid, leaves king in check
        "END"
      )
    }

    val res = App.run().run(fakeEnv).run(GameState.beginning).value.unsafeRunSync()
    assert(res.isRight)
    assert(fakeEnv.linesWritten.contains("Your King is in check, play wise!"))
    assert(fakeEnv.linesWritten.contains("Move is invalid, you can't leave your king in check!"))
  }

}
