

package bancor

import java.math.BigInteger

class TockerChangerManager {
  var tokenMap = Map.empty[String, BancorToken]
  def totalSupply = {
    val total = tokenMap.values.map(_.supply).sum
    if (total == 9223372036854775807L) throw new IllegalStateException(s"total supply $total illegal")
    total
  }

  def addBancorToken(token: BancorToken) = {
    assert(!tokenMap.contains(token.id))
    tokenMap += token.id -> token
  }

  def changer(dir: (String, String)) = new TokenChanger(tokenMap(dir._1), tokenMap(dir._2))
  def token(id: String) = tokenMap(id)

  override def toString() = s"token supply: $totalSupply,\n${tokenMap.values.mkString("\n")}"
}

class TokenChanger(src: BancorToken, dest: BancorToken) {
  def convert(amount: Long) = {
    val tokenAmount = src.buySmartToken(amount)
    val destAmount = dest.sellSmartToken(tokenAmount)
    println(s"summary: $amount ${src.id} -> $tokenAmount TOKEN -> $destAmount ${dest.id}")
    destAmount
  }

  def price = dest.price / src.price
}

case class BancorToken(
  val id: String,
  var supply: Long,
  var reserve: Long)(implicit val manager: TockerChangerManager) {

  if (supply <= 0 || supply == 9223372036854775807L || reserve <= 0 || reserve == 9223372036854775807L)
    throw new IllegalArgumentException(s"supply $supply reserve $reserve illegal")

  lazy val crr = supply.toDouble / manager.totalSupply
  def price = reserve.toDouble / (manager.totalSupply * crr)

  def buySmartToken(reserveAmount: Long, perform: Boolean = true): Long = {
    assert(reserveAmount >= 0) // for now
    val tokenAmount = reserve2smart(reserveAmount) - 1
    if (tokenAmount <= 0 || reserveAmount <= 0) 0
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
    val reserveAmount = -smart2reserve(-tokenAmount) - 1
    if (tokenAmount <= 0 || reserveAmount <= 0) 0
    else {
      if (perform) {
        supply -= tokenAmount
        reserve -= reserveAmount
      }
      reserveAmount
    }
  }

  private def smart2reserve(amount: Long) = {
    val result = Math.floor((Math.pow(1 + amount.toDouble / manager.totalSupply, 1 / crr) - 1) * reserve).toLong
    // println(s"$amount token converted to $result $id (= Math.floor((Math.pow(1 + ${amount}.0 / ${manager.totalSupply}, 1 / $crr) - 1) * $reserve))")
    result
  }
  private def reserve2smart(amount: Long) = {
    val result = Math.floor((Math.pow(1 + amount.toDouble / reserve, crr) - 1) * manager.totalSupply).toLong
    // println(s"$amount $id converted to $result token (= Math.floor((Math.pow(1 + ${amount}.0 / $reserve, $crr) - 1) * ${manager.totalSupply}))")
    result
  }

  override def toString() = s"$id reserve: $reserve, crr: $crr, price: $price}}"
}

object Main extends App {

  implicit val manager = new TockerChangerManager()
  // manager.addBancorToken(BancorToken("AAA", 1E9.toLong, 1E11.toLong)) // 1000 BTC and 10000 CNY each
  // manager.addBancorToken(BancorToken("BBB", 1E9.toLong, 1E11.toLong))
  // manager.addBancorToken(BancorToken("CCC", 1E9.toLong, 1E11.toLong))

  val supplySize = 1E18.toLong
  val reserveSize = 1E18.toLong

  if (supplySize < 0 || supplySize == Long.MaxValue || reserveSize < 0 || supplySize == Long.MaxValue) {
    sys.exit(1)
  }

  manager.addBancorToken(BancorToken("AAA", supplySize, reserveSize))
  manager.addBancorToken(BancorToken("BBB", supplySize, reserveSize))
  manager.addBancorToken(BancorToken("CCC", supplySize, reserveSize))
  manager.addBancorToken(BancorToken("DDD", supplySize, reserveSize))

  val aaa2bbb = manager.changer("AAA" -> "BBB")
  val bbb2aaa = manager.changer("BBB" -> "AAA")

  println("\nOriginal State:")
  println(manager)
  println("\n\n")

  (1 to 100) foreach { i =>
    // println("=" * 50)
    // print("Iteration #" + i + "   ")
    val x = 10000000L
    // println("price:  " + aaa2bbb.price)
    val t = aaa2bbb.convert(x)

    // println(manager)
    // println("-" * 50)
    // val y = bbb2aaa.convert(t)

    // println(manager)
    // println("-" * 50)

    // println(s"trade lost: $x - $y = ${x - y}\n\n")

  }

  println(manager)
  println("\n\n")

  (1 to 1) foreach { i =>
    // println("=" * 50)
    print("Iteration #" + i + "   ")
    val x = 500000000L
    println("price:  " + bbb2aaa.price)
    val t = bbb2aaa.convert(x)

    // println(manager)
    // println("-" * 50)
    // val y = bbb2aaa.convert(t)

    // println(manager)
    // println("-" * 50)

    // println(s"trade lost: $x - $y = ${x - y}\n\n")

  }

  println(manager)
  println("\n\n")
}
