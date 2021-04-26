package scala2021.skarasik.task02

import scala.annotation.tailrec

object Task02 {
  def main(args: Array[String]): Unit = {
    val tests = Seq(
      ("if((2+x)*(3-y)==3)", true),
      ("Я сказал ему (это еще (не) сделано). (Но он не послушал)", true),
      (":-)", false),
      ("())(", false)
    )

    tests.foreach(test => {
      val (string, expected) = test
      assert(
        checkParenthesesBalance(string.toList) == expected,
        "Test \"" + string + "\" is failed!")
    })

    println("All tests are passed.")
  }

  def checkParenthesesBalance(charList: List[Char]): Boolean = {
    @tailrec
    def checkRecursive(values: List[Char], openCount: Int): Boolean = values match {
      case current :: nextValues =>
        current match {
          case '(' => checkRecursive(nextValues, openCount + 1)
          case ')' => if (openCount == 0) false else checkRecursive(nextValues, openCount - 1)
          case _ => checkRecursive(nextValues, openCount)
        }
      case _ => openCount == 0
    }

    checkRecursive(charList, 0)
  }
}
