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
}
