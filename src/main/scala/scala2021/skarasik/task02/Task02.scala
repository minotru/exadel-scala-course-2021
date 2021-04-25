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
      val actual = checkParenthesesBalance(string.toList)
      assert(
        checkParenthesesBalance(string.toList) == expected,
        "Test \"" + string + "\" is failed!")
    })

    println("All tests are passed.")
  }

  def checkParenthesesBalance(charList: List[Char]): Boolean = {
    @tailrec
    def checkRecursive(iter: Iterator[Char], openCount: Int): Boolean = {
      if (iter.hasNext) {
        iter.next() match {
          case '(' => checkRecursive(iter, openCount + 1)
          case ')' => if (openCount == 0) false else checkRecursive(iter, openCount - 1)
          case _ => checkRecursive(iter, openCount)
        }
      } else {
        openCount == 0
      }
    }

    checkRecursive(charList.iterator, 0)
  }
}
