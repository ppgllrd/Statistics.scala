/** ****************************************************************************
  * Implementation of different descriptive statistics .
  * These are private and assumed to avoid checking redundant
  * preconditions.
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package statistics.descriptive

import scala.language.experimental.macros

package object internals {
  private[statistics] def sum_(data: Array[Int]): Int =
  macro templates.Sums.sum[Int]

  private[statistics] def sum_(data: Array[Double]): Double =
  macro templates.Sums.sum[Double]

  private[statistics] def product_(data: Array[Int]): Int =
  macro templates.Product.product[Int]

  private[statistics] def product_(data: Array[Double]): Double =
  macro templates.Product.product[Double]

  private[statistics] def max_(data: Array[Int]): Int =
    macro templates.Max.max[Int]

  private[statistics] def max_(data: Array[Double]): Double =
    macro templates.Max.max[Double]

  private[statistics] def min_(data: Array[Int]): Int =
    macro templates.Min.min[Int]

  private[statistics] def min_(data: Array[Double]): Double =
    macro templates.Min.min[Double]

  private[statistics] def sumSqr_(data: Array[Int]): Int =
    macro templates.Sums.sumSqr[Int]

  private[statistics] def sumSqr_(data: Array[Double]): Double =
    macro templates.Sums.sumSqr[Double]

  private[statistics] def mean_(data: Array[Int]): Double =
    sum_(data).toDouble / data.length

  private[statistics] def mean_(data: Array[Double]): Double =
    sum_(data) / data.length

  private[statistics] def variance_(mean: Double, data: Array[Int]): Double = {
    (sumSqr_(data) - data.length * mean * mean) / (data.length - 1)
  }

  private[statistics] def variance_(mean: Double, data: Array[Double]): Double = {
    (sumSqr_(data) - data.length * mean * mean) / (data.length - 1)
  }

  private[statistics] def variance_(data: Array[Int]): Double = {
    variance_(internals.mean_(data), data)
  }

  private[statistics] def variance_(data: Array[Double]): Double = {
    variance_(internals.mean_(data), data)
  }

  private[statistics] def standardDeviation_(mean: Double, data: Array[Int]): Double = {
    math.sqrt(variance_(mean, data))
  }

  private[statistics] def standardDeviation_(mean: Double, data: Array[Double]): Double = {
    math.sqrt(variance_(mean, data))
  }

  private[statistics] def standardDeviation_(data: Array[Int]): Double = {
    math.sqrt(variance_(data))
  }

  private[statistics] def standardDeviation_(data: Array[Double]): Double = {
    math.sqrt(variance_(data))
  }

  private[statistics] def variancePopulation_(mean: Double, data: Array[Int]): Double = {
    (sumSqr_(data) - data.length * mean * mean) / data.length
  }

  private[statistics] def variancePopulation_(mean: Double, data: Array[Double]): Double = {
    (sumSqr_(data) - data.length * mean * mean) / data.length
  }

  private[statistics] def variancePopulation_(data: Array[Int]): Double = {
    variancePopulation_(internals.mean_(data), data)
  }

  private[statistics] def variancePopulation_(data: Array[Double]): Double = {
    variancePopulation_(internals.mean_(data), data)
  }

  private[statistics] def standardDeviationPopulation_(mean: Double, data: Array[Int]): Double = {
    math.sqrt(variancePopulation_(mean, data))
  }

  private[statistics] def standardDeviationPopulation_(mean: Double, data: Array[Double]): Double = {
    math.sqrt(variancePopulation_(mean, data))
  }

  private[statistics] def standardDeviationPopulation_(data: Array[Int]): Double = {
    math.sqrt(variancePopulation_(data))
  }

  private[statistics] def standardDeviationPopulation_(data: Array[Double]): Double = {
    math.sqrt(variancePopulation_(data))
  }

  private[statistics] def midRange_(data: Array[Int]): Double = {
    (max_(data) + min_(data)) / 2.0
  }

  private[statistics] def midRange_(data: Array[Double]): Double = {
    (max_(data) + min_(data)) / 2.0
  }
}
