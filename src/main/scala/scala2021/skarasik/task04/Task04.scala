package scala2021.skarasik.task04

import scala.annotation.tailrec

object Task04 {
  def main(args: Array[String]): Unit = {
    val testCases = Seq(
      (List(2, 4, 6), 8, true),
      (List(2, 4 ,6), 5, false)
    )

    for (testCase <- testCases) {
      val (coins, change, expected) = testCase
      assert(canGiveChangeBruteForce(coins, change) == expected,
        s"Failed case: ${testCase}")
    }

    println("All tests are passed.")
  }

  def canGiveChangeBruteForce(coins: List[Int], change: Int): Boolean = {
    // a brute force solution - generate all subsets (2^N) an check the sum.
    // works with 32 coins at most (there are 32 in bits in Int).
    
    @tailrec
    def _canGiveChangeBruteForce(coins: List[Int], change: Int, mask: Int): Boolean = {
      val sum = mask
        .toBinaryString
        .take(coins.length)
        .map(digit => if (digit == '1') 1 else 0)
        .zip(coins)
        .foldLeft(0)((s, p) => s + p._1 * p._2)

      if (sum == change) {
        true
      }
      else if (mask == (1 << coins.length) - 1) {
        false
      }
      else {
        _canGiveChangeBruteForce(coins, change, mask + 1)
      }
    }

    coins.contains(change) || _canGiveChangeBruteForce(coins, change, 1)
  }
}
