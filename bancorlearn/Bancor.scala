

package bancor

class TockerChangerManager {
  var tokenMap = Map.empty[String, BancorToken]
  def totalSupply = tokenMap.values.map(_.supply).sum

  def addBancorToken(token: BancorToken) = {
    assert(!tokenMap.contains(token.id))
    tokenMap += token.id -> token
  }

  def changer(dir: (String, String)) = new TokenChanger(tokenMap(dir._1), tokenMap(dir._2))
  def token(id: String) = tokenMap(id)

  override def toString() = s"totalSupply: $totalSupply,\n${tokenMap.values.mkString("\n")}"
}

class TokenChanger(src: BancorToken, dest: BancorToken) {
  def convert(amount: Long) = {
    val tokenAmount = src.buySmartToken(amount)
    val destAmount = dest.sellSmartToken(tokenAmount)
    println(s"operation: $amount ${src.id} -> $tokenAmount TOKEN -> $destAmount ${dest.id}")
    destAmount
  }

  def price = dest.price / src.price
}

case class BancorToken(
  val id: String,
  var supply: Long,
  var reserve: Long)(implicit val manager: TockerChangerManager) {

  lazy val crr = supply.toDouble / manager.totalSupply
  def price = reserve.toDouble / supply//(manager.totalSupply * crr)

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

  override def toString() = s"$id reserve: $reserve, token supply: $supply, crr: $crr, price: $price}}"
}

object Main extends App {

  implicit val manager = new TockerChangerManager()
  manager.addBancorToken(BancorToken("AAA", 1E9.toLong, 1E11.toLong)) // 1000 BTC and 10000 CNY each
  manager.addBancorToken(BancorToken("BBB", 1E9.toLong, 1E11.toLong))
  manager.addBancorToken(BancorToken("CCC", 1E9.toLong, 1E11.toLong))

  val aaa2bbb = manager.changer("AAA" -> "BBB")
  val bbb2aaa = manager.changer("BBB" -> "AAA")

  println(manager)
  println("-------------------------------------------")

  (1 to 1) foreach { i =>
    val x = 10000000L
    val t = aaa2bbb.convert(x)

    println(manager)
    println("-------------------------------------------")
    val y = bbb2aaa.convert(t)

    println(manager)
    println("-------------------------------------------")

    println(s"gain: $y - $x = ${y - x}")
  }
}