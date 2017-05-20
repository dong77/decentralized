package io.dong.makermaker

case class Order(amountA: Double /*out*/ , amountB: Double /*in*/ ) {
  lazy val rate = amountA.toDouble / amountB
}

object Main extends App {
  val orders = Seq(Order(10000, 10), Order(10, 100), Order(100, 10000))
  orders.foreach(println)

  val discount = 1 - Math.pow(1 / orders.map(_.rate).reduce(_ * _), 1.0 / orders.size)
  val size = orders.size

  var idx = 0;
  var amountA = orders(0).amountA

  (0 until size*2) foreach { i =>
  	println("")

    val o1 = orders(i % size)
    val o2 = orders((i + 1) % size)

    val rate1 = o1.rate * (1 - discount)
    val amountB1 = amountA / rate1

    println(s"$i: $o1 ${amountA.toLong} => ${amountB1.toLong} ($rate1)")

    if (amountB1 > o2.amountA) {
      idx =i+1 
      amountA = o2.amountA
      println("found smaller: " + idx)
    } else {
      amountA = amountB1
    }

  }

}
