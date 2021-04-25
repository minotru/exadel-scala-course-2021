package scala2021.skarasik.task03

object Task03 {
  def main(args: Array[String]): Unit = {
    val input = "aaaabccaadeeeee"
    val output = encodeDirect(input)
    println(output)
  }

  def encodeDirect(data: Iterable[Char]): List[(Int, Char)] = {
    def encodeStep(encodedData: Seq[(Int, Char)], symbol: Char): Seq[(Int, Char)] = {
      if (encodedData.isEmpty || encodedData.last._2 != symbol) {
        encodedData.appended((1, symbol))
      } else {
        // might be memory inefficient?
        encodedData.init.appended((encodedData.last._1 + 1, symbol))
      }
    }

    data.foldLeft(Seq[(Int, Char)]())(encodeStep).toList
  }
}
