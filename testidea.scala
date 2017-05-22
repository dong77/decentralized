package io.dong.makermaker

case class Order(amountA: Long /*out*/ , amountB: Long /*in*/ ) {
  lazy val rate = amountA.toDouble / amountB
}

case class OrderX(order: Order, discount: Double, feePercentage: Double = 0.1) {
  var amountA: Long = order.amountA
  var amountB: Long = order.amountB

  lazy val actualRate = order.rate * (1 - discount)
  lazy val fee = ((amountB - amountA / order.rate) * feePercentage).toLong

  override def toString() = s"$order [ $amountA / $actualRate => ${amountB - fee}+$fee(fee))]"
}

object Main extends App {
  val orders = Seq(Order(10000 * 1000, 10 * 1000), Order(10 * 1000, 100 * 1000), Order(100 * 1000, 8000 * 1000))

  val discount = 1 - Math.pow(1 / orders.map(_.rate).reduce(_ * _), 1.0 / orders.size)
  val orders2 = orders.map { order => OrderX(order, discount) }

  val size = orders.size

  var bottleNetIndex = 0;
  var amountA = orders2(0).amountA

  def calculate(i: Int) = {
    val o1 = orders2(i % size)
    val o2 = orders2((i + 1) % size)

    o1.amountB = (amountA / o1.actualRate).toLong

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

  orders2.foreach(println)
}
