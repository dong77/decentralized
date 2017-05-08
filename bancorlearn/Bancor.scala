

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

  def changer(dir: (String, String)) = new TokenChanger(tokenMap(dir._1), tokenMap(dir._2))

  override def toString() = s"totalTokens: $totalTokens,\n${tokenMap.values.mkString("\n")}"
}

class TokenChanger(src: BancorToken, dest: BancorToken) {
  def apply(amount: Long) = {
    val tokenAmount = src.buySmartToken(amount)
    val destAmount = dest.sellSmartToken(tokenAmount)
    println(s"$amount ${src.id}->$tokenAmount->$destAmount ${dest.id} @${ destAmount * 1.0 / amount}")
    destAmount
  }

  def price(amount: Long): Option[Double] = {
    println("srcAmount: " + amount)
    val tokenAmount = src.buySmartToken(amount, false)
    println("tokenAmount: " + tokenAmount)
    val destAmount = dest.sellSmartToken(tokenAmount, false)
    println("destAmount: " + destAmount)
    if (tokenAmount == 0 || destAmount == 0) None
    else Some(destAmount * 1.0 / amount)
  }

  def price = dest.price / src.price

  def reverse = new TokenChanger(dest, src)
}

case class BancorToken(
  val id: String,
  var supply: Long,
  var reserve: Long,
  var crr: Double = 1.0) {
  def price = reserve / (supply * crr)

  // Buy smart token with reserve token
  def buySmartToken(reserveAmount: Long, perform: Boolean = true): Long = {
    assert(reserveAmount >= 0) // for now
    val tokenAmount = reserve2smart(reserveAmount)
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
    assert(tokenAmount >= 0) // for now
    val reserveAmount = smart2reserve(tokenAmount)
    if (tokenAmount == 0 || reserveAmount == 0) 0
    else {
      if (perform) {
        supply -= tokenAmount
        reserve -= reserveAmount
      }
      reserveAmount
    }
  }

  private def smart2reserve(amount: Long) = Math.floor((Math.pow(1 + amount.toDouble / supply, 1 / crr) - 1) * reserve).toLong
  private def reserve2smart(amount: Long) = Math.floor((Math.pow(1 + amount.toDouble / reserve, crr) - 1) * supply).toLong

  override def toString() = s"$id: $reserve, supply: $supply, crr: $crr, price: $price}}"
}

// case class LMSR(implicit val b: Double) {

//   def cost(q1: Double, q2: Double)(implicit b: Double) = {
//     b * Math.log(Math.pow(Math.E, q1 / b) + Math.pow(Math.E, q2 / b))
//   }
// }

object Main extends App {

  val manager = new TockerChangerManager()
  manager.addBancorToken(BancorToken("BTC", 1E10.toLong, 5E4.toLong))
  manager.addBancorToken(BancorToken("LTC", 1E10.toLong, 1E5.toLong))

  println(manager)
  val changer1 = manager.changer("BTC"-> "LTC")
  val changer2 = changer1.reverse

  (1 to 10) foreach { i =>
    val x = 5000L
    val t = changer1(x)
    val y = changer2(t)
    println(s"$x ~ $y\n")
  }

  println("---")
  println(manager)
}