package scala2021.skarasik.task04

import scala.annotation.tailrec

object Task04 {
  def main(args: Array[String]): Unit = {
    val testCases = Seq(
      (Seq(2, 4, 6), 8, true),
      (Seq(2, 4 ,6), 5, false)
    )

    println("Brute force solution")
    for (testCase <- testCases) {
      val (coins, change, expected) = testCase
      assert(canGiveChangeBruteForce(coins, change) == expected,
        s"Failed case: ${testCase}")
    }
    println("All tests are passed.")

    println("Recursive solution")
    for (testCase <- testCases) {
      val (coins, change, expected) = testCase
      assert(canGiveChangeRecursive(coins, change) == expected,
        s"Failed case: ${testCase}")
    }
    println("All tests are passed.")
  }

  def canGiveChangeRecursive(coins: Seq[Int], change: Int): Boolean = {
    // this is a pure recursive solution (dynamic programming)
    // but it is very ineffective.
    // we need to maintain cache for already precomputed values Q(i, s)
    // but cache needs to be shared across calls and to be a **mutable** structure.
    // whereas we avoid mutable structures.
    def Q(i: Int, s: Int): Boolean = {
      if (i == 0) {
        coins(i) == s
      } else {
        (coins(i) == s) || Q(i - 1, s) || Q(i - 1, s - coins(i))
      }
    }

    Q(coins.length - 1, change)
  }

  def canGiveChangeBruteForce(coins: Seq[Int], change: Int): Boolean = {
    // a brute force solution - generate all subsets (2^N) an check the sum.
    // works with 32 coins at most (there are 32 in bits in Int).
    
    @tailrec
    def _canGiveChangeBruteForce(coins: Seq[Int], change: Int, mask: Int): Boolean = {
      val sum = coins
        .zipWithIndex
        .foldLeft(0)((sum, p) => {
          val (coin, i) = p
          if (((1 << i) & mask) > 0) sum + coin else sum
        })

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
