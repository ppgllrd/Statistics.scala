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

  def product(data: Array[Int]): Int = {
    assert(data.nonEmpty, "product: data must be non-empty")
    internals.product_(data)
  }

  def product(data: Array[Double]): Double = {
    assert(data.nonEmpty, "product: data must be non-empty")
    internals.product_(data)
  }

  def max(data: Array[Int]): Int = {
    assert(data.nonEmpty, "max: data must be non-empty")
    internals.max_(data)
  }

  def max(data: Array[Double]): Double = {
    assert(data.nonEmpty, "max: data must be non-empty")
    internals.max_(data)
  }

  def min(data: Array[Int]): Int = {
    assert(data.nonEmpty, "min: data must be non-empty")
    internals.min_(data)
  }

  def min(data: Array[Double]): Double = {
    assert(data.nonEmpty, "min: data must be non-empty")
    internals.min_(data)
  }

  def mean(data: Array[Int]): Double = {
    assert(data.nonEmpty, "mean: data must be non-empty")
    internals.mean_(data)
  }

  def mean(data: Array[Double]): Double = {
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

  def standardDeviation(data: Array[Int]): Double = {
    assert(data.nonEmpty, "standardDeviation: data must be non-empty")
    internals.standardDeviation_(data)
  }

  def standardDeviation(data: Array[Double]): Double = {
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

  def standardDeviationPopulation(data: Array[Int]): Double = {
    assert(data.nonEmpty, "standardDeviationPopulation: data must be non-empty")
    internals.standardDeviationPopulation_(data)
  }

  def standardDeviationPopulation(data: Array[Double]): Double = {
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
}
