package scala2021.skarasik.task01

object Main {
  def main(args: Array[String]) {
    val counts = Array(
      "900,google.com",
      "60,mail.yahoo.com",
      "10,mobile.sports.yahoo.com",
      "40,sports.yahoo.com",
      "10,stackoverflow.com",
      "2,en.wikipedia.org",
      "1,es.wikipedia.org",
      "1,mobile.sports"
    )

    val domainVisits = countDomainVisits(counts)
    val sortedDomainVisits = domainVisits.sortBy(_._2.reverse)

    for ((domainCount, domainName) <- sortedDomainVisits) {
      println(domainCount, domainName)
    }
  }

  def countDomainVisits(csvLines: Seq[String]): Seq[(Int, String)] = {
    def extractSubdomains(domain: String): Seq[String] = {
      val subdomains = domain
        .zipWithIndex
        .filter(_._1 == '.')
        .map(_._2)
        .map(idx => domain.substring(idx + 1))

      subdomains :+ domain
    }

    csvLines
      .map(csvLine => {
        val tokens = csvLine.split(',')
        (tokens(0).toInt, tokens(1))
      })
      .flatMap(item => {
        val (count, domain) = item
        extractSubdomains(domain).map((count, _))
      })
      .groupBy(_._2)
      .view.mapValues(_.map(_._1).sum)
      .map(pair => (pair._2, pair._1))
      .toSeq
  }
}
