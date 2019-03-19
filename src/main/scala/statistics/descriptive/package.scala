/** ****************************************************************************
  * Public interface for descriptive statistics
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package statistics

package object descriptive {
  def sum(data: Array[Int]): Int = {
    assert(data.nonEmpty, "sum: data must be non-empty")
    internals.sum_(data)
  }

  def sum(data: Array[Double]): Double = {
    assert(data.nonEmpty, "sum: data must be non-empty")
    internals.sum_(data)
  }

  def sum[A](data: Array[A])(implicit num: Numeric[A]): A = {
    assert(data.nonEmpty, "sum: data must be non-empty")
    internals.sum_(data)
  }


  def product(data: Array[Int]): Int = {
    assert(data.nonEmpty, "product: data must be non-empty")
    internals.product_(data)
  }

  def product(data: Array[Double]): Double = {
    assert(data.nonEmpty, "product: data must be non-empty")
    internals.product_(data)
  }

  def product[A](data: Array[A])(implicit num: Numeric[A]): A = {
    assert(data.nonEmpty, "product: data must be non-empty")
    internals.product_(data)
  }


  def min(data: Array[Int]): Int = {
    assert(data.nonEmpty, "min: data must be non-empty")
    internals.min_(data)
  }

  def min(data: Array[Double]): Double = {
    assert(data.nonEmpty, "min: data must be non-empty")
    internals.min_(data)
  }

  def min[A](data: Array[A])(implicit ord: Ordering[A]): A = {
    assert(data.nonEmpty, "min: data must be non-empty")
    internals.min_(data)(ord)
  }


  def max(data: Array[Int]): Int = {
    assert(data.nonEmpty, "max: data must be non-empty")
    internals.max_(data)
  }

  def max(data: Array[Double]): Double = {
    assert(data.nonEmpty, "max: data must be non-empty")
    internals.max_(data)
  }

  def max[A](data: Array[A])(implicit ord: Ordering[A]): A = {
    assert(data.nonEmpty, "max: data must be non-empty")
    internals.max_(data)(ord)
  }

/*
  minMax algorithm turns out to be slower than min and max

  def minMax(data: Array[Int]): (Int, Int) = {
    assert(data.nonEmpty, "minMax: data must be non-empty")
    internals.minMax_(data)
  }

  def minMax(data: Array[Double]): (Double, Double) = {
    assert(data.nonEmpty, "minMax: data must be non-empty")
    internals.minMax_(data)
  }

  def minMax[A](data: Array[A])(implicit ord: Ordering[A]): (A, A) = {
    assert(data.nonEmpty, "minMax: data must be non-empty")
    internals.minMax_(data)(ord)
  }
*/

  def mean(data: Array[Int]): Double = {
    assert(data.nonEmpty, "mean: data must be non-empty")
    internals.mean_(data)
  }

  def mean(data: Array[Double]): Double = {
    assert(data.nonEmpty, "mean: data must be non-empty")
    internals.mean_(data)
  }

  def mean[A](data: Array[Double])(implicit num: Numeric[A]): Double = {
    assert(data.nonEmpty, "mean: data must be non-empty")
    internals.mean_(data)
  }


  def variance(data: Array[Int]): Double = {
    assert(data.nonEmpty, "variance: data must be non-empty")
    internals.variance_(data)
  }

  def variance(data: Array[Double]): Double = {
    assert(data.nonEmpty, "variance: data must be non-empty")
    internals.variance_(data)
  }

  def variance[A](data: Array[A])(implicit num: Numeric[A]): Double = {
    assert(data.nonEmpty, "variance: data must be non-empty")
    internals.variance_(data)
  }


  def standardDeviation(data: Array[Int]): Double = {
    assert(data.nonEmpty, "standardDeviation: data must be non-empty")
    internals.standardDeviation_(data)
  }

  def standardDeviation(data: Array[Double]): Double = {
    assert(data.nonEmpty, "standardDeviation: data must be non-empty")
    internals.standardDeviation_(data)
  }

  def standardDeviation[A](data: Array[A])(implicit num: Numeric[A]): Double = {
    assert(data.nonEmpty, "standardDeviation: data must be non-empty")
    internals.standardDeviation_(data)
  }


  def variancePopulation(data: Array[Int]): Double = {
    assert(data.nonEmpty, "variancePopulation: data must be non-empty")
    internals.variancePopulation_(data)
  }

  def variancePopulation(data: Array[Double]): Double = {
    assert(data.nonEmpty, "variancePopulation: data must be non-empty")
    internals.variancePopulation_(data)
  }

  def variancePopulation[A](data: Array[A])(implicit num: Numeric[A]): Double = {
    assert(data.nonEmpty, "variancePopulation: data must be non-empty")
    internals.variancePopulation_(data)
  }


  def standardDeviationPopulation(data: Array[Int]): Double = {
    assert(data.nonEmpty, "standardDeviationPopulation: data must be non-empty")
    internals.standardDeviationPopulation_(data)
  }

  def standardDeviationPopulation(data: Array[Double]): Double = {
    assert(data.nonEmpty, "standardDeviationPopulation: data must be non-empty")
    internals.standardDeviationPopulation_(data)
  }

  def standardDeviationPopulation[A](data: Array[A])(implicit num: Numeric[A]): Double = {
    assert(data.nonEmpty, "standardDeviationPopulation: data must be non-empty")
    internals.standardDeviationPopulation_(data)
  }


  def mode[A](data: Array[A]): A = {
    assert(data.nonEmpty, "mode: data must be non-empty")
    internals.mode(data)
  }


  def midRange(data: Array[Int]): Double = {
    assert(data.nonEmpty, "midRange: data must be non-empty")
    internals.midRange_(data)
  }

  def midRange(data: Array[Double]): Double = {
    assert(data.nonEmpty, "midRange: data must be non-empty")
    internals.midRange_(data)
  }

  def midRange[A](data: Array[A])(implicit num: Numeric[A]): Double = {
    assert(data.nonEmpty, "midRange: data must be non-empty")
    internals.midRange_(data)
  }


  def percentile(data: Array[Int], percentRank: Double): Double =
    percentile(data, percentRank, false)

  def percentile(data: Array[Double], percentRank: Double): Double =
    percentile(data, percentRank, false)

  def percentile[A](data: Array[Int], percentRank: Double)(implicit ord: Ordering[A], num: Numeric[A]): Double =
    percentile(data, percentRank, false)

  def percentile(data: Array[Int], percentRank: Double, canReshuffle: Boolean): Double =
    internals.percentile.percentileMacro(data, percentRank, canReshuffle)(null, scala.math.Numeric.IntIsIntegral)

  def percentile(data: Array[Double], percentRank: Double, canReshuffle: Boolean): Double =
    internals.percentile.percentileMacro(data, percentRank, canReshuffle)(null, scala.math.Numeric.DoubleIsFractional)

  def percentile[A](data: Array[A], percentRank: Double, canReshuffle: Boolean = true)(implicit ord: Ordering[A], num: Numeric[A]): Double =
    internals.percentile.percentileMacro(data, percentRank, canReshuffle)(ord, num)


  def median(data: Array[Int]): Double =
    median(data, false)

  def median(data: Array[Double]): Double =
    median(data, false)

  def median[A](data: Array[Int])(implicit ord: Ordering[A], num: Numeric[A]): Double =
    median(data, false)

  def median(data: Array[Int], canReshuffle: Boolean): Double =
    internals.percentile.medianMacro(data, canReshuffle)(null, scala.math.Numeric.IntIsIntegral)

  def median(data: Array[Double], canReshuffle: Boolean): Double =
    internals.percentile.medianMacro(data, canReshuffle)(null, scala.math.Numeric.DoubleIsFractional)

  def median[A](data: Array[A], canReshuffle: Boolean = true)(implicit ord: Ordering[A], num: Numeric[A]): Double =
    internals.percentile.medianMacro(data, canReshuffle)(ord, num)
}
