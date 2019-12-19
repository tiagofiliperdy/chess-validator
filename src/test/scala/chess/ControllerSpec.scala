package chess

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import userinput.MockOutput

class ControllerSpec extends AnyWordSpec with Matchers {

  "ControllerSpec" should {
    val file1 = getClass.getResource("/check-mate.txt").getPath
    val file2 = getClass.getResource("/check-invalid-move.txt").getPath

    "end game when all moves read are valid" in {
      val controller = new Controller(file1) with MockOutput

      controller.init()
      controller.messages must contain(">>> Game Ended :D")
    }

    "invalid move after check" in {
      val controller = new Controller(file2) with MockOutput

      controller.init()

      controller.messages mustNot contain(">>> Game Ended :D")
      controller.messages must contain(">>> King is in Check!!!")
      controller.messages must contain(">>> Invalid move, from: e8, to: f7, it leaves your King in check.")
    }
  }
}
