package scala2021.skarasik.task08

case class Frame(score1: Int, score2: Int) {
  val MAX_SCORE = 10

  def isStrike: Boolean = score1 == MAX_SCORE && score2 == 0

  def isSpare: Boolean = (score1 + score2) == MAX_SCORE

  def totalScore: Int = score1 + score2
}

object BowlingParser {
  def main(args: Array[String]): Unit = {
    val input = "9-|9-|9-|9-|9-|9-|9-|9-|9-|9-||"
    println(calculateScore(input))
  }

  def calculateScore(input: String): Int = {
    val frames = parseFrames(input)

    (0 until 10).map(i => {
      val frame = frames(i)

      if (frame.isStrike) {
        if (frames(i + 1).isStrike) {
          frame.totalScore + frames(i + 1).score1 + frames(i + 2).score1
        } else {
          frame.totalScore + frames(i + 1).totalScore
        }
      } else if (frame.isSpare) {
        frame.totalScore + frames(i + 1).score1
      } else {
        frame.totalScore
      }
    }).sum
  }

  private def parseFrames(input: String): Vector[Frame] = {
    val tokens = raw"\|{1,2}".r.split(input).toVector
    val frames = tokens.map(parseFrame)
    frames
  }

  private def parseFrame(token: String): Frame = {
    def parseDigit(char: Char): Int = char match {
      case char if char.isDigit => char.asDigit
      case 'X' => 10
      case '-' => 0
    }

    token.length match {
      case 0 => Frame(0, 0)
      case 1 => Frame(parseDigit(token(0)), 0)
      case 2 => {
        val (char1, char2) = (token(0), token(1))
        if (char2 == '/') {
          Frame(parseDigit(char1), 10 - parseDigit(char1))
        } else {
          Frame(parseDigit(char1), parseDigit(char2))
        }
      }
    }
  }
}
