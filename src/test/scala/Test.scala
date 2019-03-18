/** ****************************************************************************
  * Simple tests
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

object Test extends App {
  import statistics.descriptive._

  val xs = Array(1, 2, 3, 4, 5, 2)

  println(sum(xs))

  val ys = Array(1.0, 2.0, 3.0)

  println(sum(ys))

  println(mean(xs))
  println(variance(xs))
  println(standardDeviation(xs))
  println(mode(xs))

  val zs = Array.range(1, 101).map(_.toDouble)
  println(max(zs))

  println(percentile(zs, 25))
  println(median(zs))
}


object TestMax extends App {
  val n = 10000000
  val xs = Array.range(0,n).map(_.toLong)

  val t0 = System.currentTimeMillis()
  val m = statistics.descriptive.max(xs)
  val t1 = System.currentTimeMillis()
  println(t1 - t0)
}