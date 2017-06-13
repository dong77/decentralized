package io.dong.makermaker

object Const {
  val RATE_AMPLIFIER = 10000000
}

case class Order(amountA: Long /*out*/ , amountB: Long /*in*/ ) {
  lazy val rate = amountA * Const.RATE_AMPLIFIER / amountB
}

case class OrderX(order: Order, discount: Long, feePercentage: Double = 0.1) {
  var amountA: Long = order.amountA
  var amountB: Long = order.amountB

  lazy val actualRate = {

    val ar = order.rate * (Const.RATE_AMPLIFIER - discount) / Const.RATE_AMPLIFIER
    println("rate: " + order.rate + " actual rate is " + ar)
    ar
  }

  def savings = (amountB * order.rate - amountA * Const.RATE_AMPLIFIER) / Const.RATE_AMPLIFIER

  def fee = {
    val f = Math.floor(savings * feePercentage).toLong
    Math.min(order.amountA - amountA, f)
    // 0
  }
  def remaningAmountA = order.amountA - amountA - fee
  def remaningAmountB = remaningAmountA * Const.RATE_AMPLIFIER / order.rate

  override def toString() = s"$order [ $amountA / $actualRate => ${amountB} $fee(fee))]"

  def updatedOrder = {
    println(s"paid $amountA and fee $fee")
    Order(remaningAmountA, remaningAmountB)
  }
}

object Main extends App {
  val orders = Seq(Order(10000 * 1000, 10 * 1000), Order(9 * 1000, 110 * 1000), Order(100 * 1000, 8000 * 1000))

  val discount = ((1 - Math.pow(1 / orders.map(_.rate.toDouble / Const.RATE_AMPLIFIER).reduce(_ * _), 1.0 / orders.size)) * Const.RATE_AMPLIFIER).toLong

  println("discount: " + discount)
  val orders2 = orders.map { order => OrderX(order, discount) }

  val size = orders.size

  var bottleNetIndex = 0;
  var amountA = orders2(0).amountA

  def calculate(i: Int) = {
    val o1 = orders2(i % size)
    val o2 = orders2((i + 1) % size)

    o1.amountB = amountA * Const.RATE_AMPLIFIER / o1.actualRate

    if (o1.amountB > o2.amountA) {
      bottleNetIndex = i + 1
    } else {
      o2.amountA = o1.amountB
    }
    amountA = o2.amountA
  }

  (0 until size) foreach (calculate)

  println(s"Bottleneck idx found: $bottleNetIndex")

  (0 to bottleNetIndex) foreach (calculate)

  orders2.foreach { x =>
    println(x)
    println(x.updatedOrder)
  }

}
