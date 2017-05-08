

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
  def apply(amount: Long) = {
     // println(".......")
    val tokenAmount = src.buySmartToken(amount)
    // println(".......")
    val destAmount = dest.sellSmartToken(tokenAmount)
     // println("...d....")
    println(s"operation: $amount ${src.id} -> $tokenAmount TOKEN -> $destAmount ${dest.id}")
    destAmount 
  }

  // def price(amount: Long): Option[Double] = {
  //   println("srcAmount: " + amount)
  //   val tokenAmount = src.buySmartToken(amount, false)
  //   println("tokenAmount: " + tokenAmount)
  //   val destAmount = dest.sellSmartToken(tokenAmount, false)
  //   println("destAmount: " + destAmount)
  //   if (tokenAmount == 0 || destAmount == 0) None
  //   else Some(destAmount * 1.0 / amount)
  // }

  def price = dest.price / src.price

  def reverse = new TokenChanger(dest, src)
}

case class BancorToken(
  val id: String,
  var supply: Long,
  var reserve: Long
 )(implicit val manager:TockerChangerManager) {


  def price = reserve.toDouble / supply
  lazy val crr= supply.toDouble / manager.totalSupply

  // Buy smart token with reserve token
  def buySmartToken(reserveAmount: Long, perform: Boolean = true): Long = {
    assert(reserveAmount >= 0) // for now
    val tokenAmount = reserve2smart(reserveAmount)
    if (tokenAmount == 0 || reserveAmount == 0) 0
    else {
      if (perform) {
        supply += tokenAmount
        reserve += reserveAmount
        // println(s"$id supply= $supply, reserve= $reserve")
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
        // println(s"$id supply= $supply, reserve= $reserve")
      }
      reserveAmount
    }
  }

  private def smart2reserve(amount: Long) = Math.floor((Math.pow(1 + amount.toDouble / supply, 1 / crr) - 1) * reserve).toLong
  private def reserve2smart(amount: Long) = Math.floor((Math.pow(1 + amount.toDouble / reserve, crr) - 1) * supply).toLong

  override def toString() = s"$id reserve: $reserve, token supply: $supply, crr: $crr, price: $price}}"
}

// case class LMSR(implicit val b: Double) {

//   def cost(q1: Double, q2: Double)(implicit b: Double) = {
//     b * Math.log(Math.pow(Math.E, q1 / b) + Math.pow(Math.E, q2 / b))
//   }
// }

object Main extends App {

  implicit val manager = new TockerChangerManager()
  manager.addBancorToken(BancorToken("AAA", 1E9.toLong, 1E11.toLong)) // 1000 BTC and 10000 CNY each
  manager.addBancorToken(BancorToken("BBB", 1E9.toLong, 1E11.toLong)) 
    // manager.addBancorToken(BancorToken("ETH",1E9.toLong, 1E11.toLong)) 



  val changer1 = manager.changer("AAA"-> "BBB")
  val changer2 = changer1.reverse

  println(manager)
  println("-------------------------------------------")

  (1 to 1) foreach { i =>
    val x = 10000000L
    val t = changer1(x)

  println(manager)
  println("-------------------------------------------")
    val y = changer2(t)


  println(manager)
  println("-------------------------------------------")


println(s"gain: $y - $x = ${y-x}")
  }


  // val btc = manager.token("BTC")
  // val x = 1000
  // val t1 = btc.buySmartToken(x)
  // println(btc)
  // val x2 = btc.sellSmartToken(t1)
  // println(btc)
  // println(x + "->" + x2)
}