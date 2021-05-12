package scala2021.skarasik.task08

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import scala2021.skarasik.task08.BowlingParser

class BowlingParserSuite extends AnyFunSuite with TableDrivenPropertyChecks {
  test("calculateScore") {
    val cases = Table(
      ("input", "score"),
      ("X|X|X|X|X|X|X|X|X|X||XX", 300),
      ("9-|9-|9-|9-|9-|9-|9-|9-|9-|9-||", 90),
      ("5/|5/|5/|5/|5/|5/|5/|5/|5/|5/||5", 150),
      ("X|7/|9-|X|-8|8/|-6|X|X|X||81", 167)
    )
    forAll(cases) {
      (input, score) => {
        assert(BowlingParser.calculateScore(input) == score)
      }
    }
  }
}
