package chess.positions

import cats.data.Validated.Valid
import cats.implicits._
import cats.kernel.laws.discipline.EqTests
import chess.FpFinalSpec
import chess.board.Board
import org.scalacheck.Gen

class PositionSpec extends FpFinalSpec {

    test("create a valid position with file and rank") {
      val gen = for {
        file <- Gen.choose(Board.dimension.start, Board.dimension.end)
        rank <- Gen.choose(Board.dimension.start, Board.dimension.end)
      } yield (file, rank)

      forAll(gen) { case (file, rank) =>
        assert(Position(file, rank) eqv Valid(Position.unsafeCreate(file, rank)))
      }
    }

    test("create a valid position tuple from input") {
      case class GenPos(fileFrom: Char, rankFrom: Int, fileTo: Char, rankTo: Int) {
        override def toString: String = s"$fileFrom$rankFrom$fileTo$rankTo"

        def toTuple: (Int, Int, Int, Int) =
          toString.toCharArray.toList match {
            case f1 :: r1 :: f2 :: r2 :: Nil =>
              (f1 - 97, 56 - r1, f2 - 97, 56 - r2)
          }
      }

      val gen = for {
        fileFrom <- Gen.oneOf(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'))
        rankFrom <- Gen.choose(Board.dimension.start + 1, Board.dimension.end + 1)
        fileTo <- Gen.oneOf(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'))
        rankTo <- Gen.choose(Board.dimension.start + 1, Board.dimension.end + 1)
      } yield GenPos(fileFrom, rankFrom, fileTo, rankTo)

      forAll(gen) { genPos =>
        val (fileFrom, rankFrom, fileTo, rankTo) = genPos.toTuple

        assert(Position(genPos.toString) eqv
          Valid((
            Position.unsafeCreate(fileFrom, rankFrom),
            Position.unsafeCreate(fileTo, rankTo))))
      }
    }

    test("create invalid position with file and rank") {
      assert(Position(10,9).isInvalid)
    }

    test("create invalid position tuple from input due to less than 4 chars") {
      assert(Position("e2h").isInvalid)
    }

    test("create invalid position tuple from input due to incorrect format") {
      assert(Position("2hh4").isInvalid)
    }

    test("calculate difference between two positions") {
      val pos1 = Position.unsafeCreate(4, 6)
      val pos2 = Position.unsafeCreate(4, 4)

      pos1.diff(pos2) mustEqual (0, -2)
    }

    checkAll("Eq[Position]", EqTests[Position].eqv)
}
