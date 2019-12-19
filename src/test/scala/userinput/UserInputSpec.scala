package userinput

import chess.Controller
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UserInputSpec extends AnyWordSpec with Matchers {

  val emptyFile = getClass.getResource("/emptyFile.txt").getPath

  "UserInputSpec" should {

    "handle empty file" in {
      val controller = new Controller(emptyFile) with MockOutput

      controller.init()

      controller.messages should contain(s"File with path '$emptyFile' is empty.")
    }

    "handle non existing file" in {
      val controller = new Controller("non-existent") with MockOutput

      controller.init()

      controller.messages should contain(s"File with path 'non-existent' doesn't exist.")
    }
  }
}
