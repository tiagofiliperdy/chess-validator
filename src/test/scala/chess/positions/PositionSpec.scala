package chess.positions

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PositionSpec extends AnyWordSpec with Matchers {

  "PositionSpec" should {

    "calculate difference between two positions" in {
      val pos1 = Position(4, 6)
      val pos2 = Position(4, 4)

      pos1.diff(pos2) mustEqual (0, -2)
    }
  }
}
