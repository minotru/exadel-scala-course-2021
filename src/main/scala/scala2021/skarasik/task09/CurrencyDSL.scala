package scala2021.skarasik.task09

import scala.math.BigDecimal.RoundingMode

trait CurrencyCode;

case object USD extends CurrencyCode;
case object EUR extends CurrencyCode;
case object GBP extends CurrencyCode;

case class Currency(amount: BigDecimal, code: CurrencyCode) {
  def +(other: Currency): Currency = {
    val amount = this.amount + other.to(this.code).amount
    Currency(amount, this.code)
  }

  def to(otherCode: CurrencyCode): Currency = {
    val conversionRates = Map[(CurrencyCode, CurrencyCode), Double](
      (USD, EUR) -> 0.83,
      (USD, GBP) -> 0.71,
      (EUR, GBP) -> 0.86,
    )

    val amount = (this.code, otherCode) match {
      case pair if pair._1 == pair._2 => this.amount
      case pair if conversionRates.contains(pair) => this.amount * conversionRates(pair)
      case pair if conversionRates.contains(pair.swap) => this.amount / conversionRates(pair.swap)
      case _ => throw new IllegalArgumentException(s"Can not convert ${this.code} to ${otherCode}.")
    }

    Currency(amount, otherCode)
  }

  override def toString: String = {
    val roundedAmount = this.amount.setScale(2, RoundingMode.HALF_EVEN)
    s"${roundedAmount}(${this.code})"
  }
}

object CurrencyConverters {
   implicit class IntAsCurrency(amount: Int) {
     def apply(currencyCode: CurrencyCode): Currency = {
       Currency(amount, currencyCode)
     }
  }
}

object CurrencyDSL {
  def main(args: Array[String]): Unit = {
    import CurrencyConverters._

    val result = 42(USD) + 35(EUR)
    val resultToPound = result to GBP

    println(resultToPound)
  }
}
