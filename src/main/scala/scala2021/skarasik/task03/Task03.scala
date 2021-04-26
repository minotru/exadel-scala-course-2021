package scala2021.skarasik.task03

import scala.annotation.tailrec

object Task03 {
  def main(args: Array[String]): Unit = {
    val input = "aaaabccaadeeeee"
    val output = encodeDirect(input.toList)
    println(output)
  }

  def encodeDirect(data: List[Char]): List[(Int, Char)] = {
    @tailrec
    def encodeStep(source: List[Char], encoded: List[(Int, Char)]): List[(Int, Char)] = {
      if (source == Nil) {
        encoded
      } else {
        val (sameValues, nextSource) = source.span(_ == source.head)
        encodeStep(nextSource, encoded:::List((sameValues.length, sameValues.head)))
      }
    }

    encodeStep(data, List[(Int, Char)]())
  }
}
