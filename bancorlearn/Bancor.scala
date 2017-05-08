

package bancor

class TockerChangerManager {
  var tokenMap = Map.empty[String, BancorToken]
  var totalTokens = 0L

  def addBancorToken(token: BancorToken) = {
    assert(!tokenMap.contains(token.id))
    totalTokens += token.supply
    token.crr = token.supply * 1.0 / totalTokens

    tokenMap.foreach { case (_, t) => t.crr *= (1 - token.crr) }
    tokenMap += token.id -> token
  }

  def getTokenChanger(srcId: String, destId: String) = new TokenChanger(tokenMap(srcId), tokenMap(destId))

  override def toString() = s"totalTokens: $totalTokens,\n${tokenMap.values.mkString("\n")}"
}

class TokenChanger(src: BancorToken, dest: BancorToken) {
  def sell(amount: Long) = {
    val tokenAmount = src.buySmartToken(amount)
    val destAmount = dest.sellSmartToken(tokenAmount)
    destAmount
  }

  def quoteSellPrice(amount: Long): Option[Double] = {
    val tokenAmount = src.buySmartToken(amount, false)
    println("tokenAmount: " + tokenAmount)
    val destAmount = dest.sellSmartToken(tokenAmount, false)
    println("destAmount: " + destAmount)
    if (tokenAmount == 0 || destAmount == 0) None
    else Some(destAmount * 1.0 / amount)
  }
}

case class BancorToken(
  val id: String,
  var supply: Long,
  var reserve: Long,
  var crr: Double = 1.0) {
  def price = reserve / (supply * crr)

  // Buy smart token with reserve token
  def buySmartToken(reserveAmount: Long, perform: Boolean = true): Long = {
    // assert(reserveAmount >= 0) // for now
    val tokenAmount = r2s(reserveAmount)
    if (tokenAmount == 0 || reserveAmount == 0) 0
    else {
      if (perform) {
        supply += tokenAmount
        reserve += reserveAmount
      }
      tokenAmount
    }
  }

  def sellSmartToken(tokenAmount: Long, perform: Boolean = true): Long = {
    // assert(tokenAmount >= 0) // for now
    val reserveAmount = s2r(tokenAmount)
    if (tokenAmount == 0 || reserveAmount == 0) 0
    else {
      if (perform) {
        supply -= tokenAmount
        reserve -= reserveAmount
      }
      reserveAmount
    }
  }

  private def s2r(amount: Long) = ((Math.pow(1 + amount.toDouble / supply, 1 / crr) - 1) * reserve).toLong
  private def r2s(amount: Long) = ((Math.pow(1 + amount.toDouble / reserve, crr) - 1) * supply).toLong

  override def toString() = s"$id: $reserve, supply: $supply, crr: $crr, price: $price}}"
}

// case class LMSR(implicit val b: Double) {

//   def cost(q1: Double, q2: Double)(implicit b: Double) = {
//     b * Math.log(Math.pow(Math.E, q1 / b) + Math.pow(Math.E, q2 / b))
//   }
// }

object Main extends App {

  val manager = new TockerChangerManager()
  manager.addBancorToken(BancorToken("BTC", 1000000000000L, 500000L))
  manager.addBancorToken(BancorToken("LTC", 1000000000000L, 50000000L))

  println(manager)
  println(manager.getTokenChanger("BTC", "LTC").quoteSellPrice(1000))
  println(manager)
}