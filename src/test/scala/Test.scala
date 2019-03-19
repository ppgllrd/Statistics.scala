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

/*
object TestMinMax extends App {
  val rnd = new scala.util.Random()

  var tMinMax = 0L
  var t = 0L

  val nTests = 1000
  val maxSize = 1000000
  for(_ <- 0 until nTests) {
    val size = maxSize/2 + rnd.nextInt(maxSize/2)
    val data = new Array[Int](size)
    for(i <- 0 until size)
      data(i) = rnd.nextInt()

    val tMinMax0 = System.currentTimeMillis()
    val (mini, maxi) = statistics.descriptive.minMax(data)
    val tMinMax1 = System.currentTimeMillis()
    tMinMax += (tMinMax1 - tMinMax0)

    val t0 = System.currentTimeMillis()
    val mini2 = statistics.descriptive.min(data)
    val maxi2 = statistics.descriptive.max(data)
    val t1 = System.currentTimeMillis()
    t += (t1 - t0)

    if(maxi != maxi2 || mini != mini2)
      sys.error(s"Failed! $mini $mini2  $maxi $maxi2")
  }
  println(tMinMax, t)
}
*/