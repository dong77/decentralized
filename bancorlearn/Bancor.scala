

package bancor

case class Market(
  initialSupply: Long,
  initialReserve: Long,
  reserveRatioOpt: Option[Double] = None) {

  var supply = initialSupply
  var reserve = initialReserve
  val reserveRatio = reserveRatioOpt.getOrElse(reserve * 1.0 / supply)
  def price = reserve / (supply * reserveRatio)

  def sell(amountOfReserveToken: Long) = {
    assert(amountOfReserveToken > 0)
    val smartToken = reserveToken2SmartToken(amountOfReserveToken)
    if (smartToken > 0) {
      supply += smartToken
      reserve += amountOfReserveToken
    }
    println(this)
    smartToken
  }

  def buy(amountOfSmartToken: Long) = {
    assert(amountOfSmartToken > 0)
    val reserveToken = smartTokenToReserveToken(amountOfSmartToken)
    if (reserveToken > 0) {
      supply -= amountOfSmartToken
      reserve -= reserveToken
    }
    println(this)
    reserveToken
  }

  def smartTokenToReserveToken(amount: Long) = ((Math.pow(1 + amount.toDouble / reserve, reserveRatio) - 1) * supply).toLong
  def reserveToken2SmartToken(amount: Long) = ((Math.pow(1 + amount.toDouble / supply, 1 / reserveRatio) - 1) * reserve).toLong

  override def toString() = s"supply $supply, reserve: $reserve, price: $price}}"
}

case class LMSR(implicit val b: Double) {

  def cost(q1: Double, q2: Double)(implicit b: Double) = {
    b * Math.log(Math.pow(Math.E, q1 / b) + Math.pow(Math.E, q2 / b))
  }
}

object Main extends App {
  var market = Market(100000000, 10000, Some(0.01))
  println(market)
  (1 to 10) foreach { i =>
    println("=======: " + i)
    val x = market.sell(6000)
    println(x)
    println(market.buy(x))
  }
  println(market)
}