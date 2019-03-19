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
  private[statistics] def sumMacro[A](data: Array[A])(num: Numeric[A]): A =
    macro templates.Sums.sum[A]

  private[statistics] def sum_(data: Array[Int]): Int =
    sumMacro(data)(null)

  private[statistics] def sum_(data: Array[Double]): Double =
    sumMacro(data)(null)

  private[statistics] def sum_[A](data: Array[A])(implicit num: Numeric[A]): A =
    sumMacro(data)(num)


  private[statistics] def sumSqrMacro[A](data: Array[A])(num: Numeric[A]): A =
    macro templates.Sums.sum[A]

  private[statistics] def sumSqr_(data: Array[Int]): Int =
    sumSqrMacro(data)(null)

  private[statistics] def sumSqr_(data: Array[Double]): Double =
    sumSqrMacro(data)(null)

  private[statistics] def sumSqr_[A](data: Array[A])(implicit num: Numeric[A]): A =
    sumSqrMacro(data)(num)


  private[statistics] def productMacro[A](data: Array[A])(num: Numeric[A]): A =
    macro templates.Product.product[A]

  private[statistics] def product_(data: Array[Int]): Int =
    productMacro(data)(null)

  private[statistics] def product_(data: Array[Double]): Double =
    productMacro(data)(null)

  private[statistics] def product_[A](data: Array[A])(implicit num: Numeric[A]): A =
    productMacro(data)(num)


  private[statistics] def minMacro[A](data: Array[A])(ord: Ordering[A]): A =
  macro templates.Min.min[A]

  private[statistics] def min_(data: Array[Int]): Int =
    minMacro(data)(null)

  private[statistics] def min_(data: Array[Double]): Double =
    minMacro(data)(null)

  private[statistics] def min_[A](data: Array[A])(implicit ord: Ordering[A]): A =
    minMacro(data)(ord)


  private[statistics] def maxMacro[A](data: Array[A])(ord: Ordering[A]): A =
    macro templates.Max.max[A]

  private[statistics] def max_(data: Array[Int]): Int =
    maxMacro(data)(null)

  private[statistics] def max_(data: Array[Double]): Double =
    maxMacro(data)(null)

  private[statistics] def max_[A](data: Array[A])(implicit ord: Ordering[A]): A =
    maxMacro(data)(ord)


  private[statistics] def minMaxMacro[A](data: Array[A])(ord: Ordering[A]): (A, A) =
    macro templates.MinMax.minMax[A]

  private[statistics] def minMax_(data: Array[Int]): (Int, Int) =
    minMaxMacro(data)(null)

  private[statistics] def minMax_(data: Array[Double]): (Double, Double) =
    minMaxMacro(data)(null)

  private[statistics] def minMax_[A](data: Array[A])(implicit ord: Ordering[A]): (A, A) =
    minMaxMacro(data)(ord)


  private[statistics] def mean_(data: Array[Int]): Double =
    sum_(data).toDouble / data.length

  private[statistics] def mean_(data: Array[Double]): Double =
    sum_(data) / data.length

  private[statistics] def mean_[A](data: Array[A])(implicit num: Numeric[A]): Double =
    num.toDouble(sum_(data)) / data.length


  private[statistics] def variance_(mean: Double, data: Array[Int]): Double = {
    (sumSqr_(data) - data.length * mean * mean) / (data.length - 1)
  }

  private[statistics] def variance_(mean: Double, data: Array[Double]): Double = {
    (sumSqr_(data) - data.length * mean * mean) / (data.length - 1)
  }

  private[statistics] def variance_[A](mean: Double, data: Array[A])(implicit num: Numeric[A]): Double = {
    (num.toDouble(sumSqr_(data)) - data.length * mean * mean) / (data.length - 1)
  }

  private[statistics] def variance_(data: Array[Int]): Double = {
    variance_(internals.mean_(data), data)
  }

  private[statistics] def variance_(data: Array[Double]): Double = {
    variance_(internals.mean_(data), data)
  }

  private[statistics] def variance_[A](data: Array[A])(implicit num: Numeric[A]): Double = {
    variance_(internals.mean_(data), data)
  }

  private[statistics] def standardDeviation_(mean: Double, data: Array[Int]): Double = {
    math.sqrt(variance_(mean, data))
  }

  private[statistics] def standardDeviation_(mean: Double, data: Array[Double]): Double = {
    math.sqrt(variance_(mean, data))
  }

  private[statistics] def standardDeviation_[A](mean: Double, data: Array[A])(implicit num: Numeric[A]): Double = {
    math.sqrt(variance_(mean, data))
  }

  private[statistics] def standardDeviation_(data: Array[Int]): Double = {
    math.sqrt(variance_(data))
  }

  private[statistics] def standardDeviation_(data: Array[Double]): Double = {
    math.sqrt(variance_(data))
  }

  private[statistics] def standardDeviation_[A](data: Array[A])(implicit num: Numeric[A]): Double = {
    math.sqrt(variance_(data))
  }


  private[statistics] def variancePopulation_(mean: Double, data: Array[Int]): Double = {
    (sumSqr_(data) - data.length * mean * mean) / data.length
  }

  private[statistics] def variancePopulation_(mean: Double, data: Array[Double]): Double = {
    (sumSqr_(data) - data.length * mean * mean) / data.length
  }

  private[statistics] def variancePopulation_[A](mean: Double, data: Array[A])(implicit num: Numeric[A]): Double = {
    (num.toDouble(sumSqr_(data)) - data.length * mean * mean) / data.length
  }

  private[statistics] def variancePopulation_(data: Array[Int]): Double = {
    variancePopulation_(internals.mean_(data), data)
  }

  private[statistics] def variancePopulation_(data: Array[Double]): Double = {
    variancePopulation_(internals.mean_(data), data)
  }

  private[statistics] def variancePopulation_[A](data: Array[A])(implicit num: Numeric[A]): Double = {
    variancePopulation_(internals.mean_(data), data)
  }



  private[statistics] def standardDeviationPopulation_(mean: Double, data: Array[Int]): Double = {
    math.sqrt(variancePopulation_(mean, data))
  }

  private[statistics] def standardDeviationPopulation_(mean: Double, data: Array[Double]): Double = {
    math.sqrt(variancePopulation_(mean, data))
  }

  private[statistics] def standardDeviationPopulation_[A](mean: Double, data: Array[A])(implicit num: Numeric[A]): Double = {
    math.sqrt(variancePopulation_(mean, data))
  }

  private[statistics] def standardDeviationPopulation_(data: Array[Int]): Double = {
    math.sqrt(variancePopulation_(data))
  }

  private[statistics] def standardDeviationPopulation_(data: Array[Double]): Double = {
    math.sqrt(variancePopulation_(data))
  }

  private[statistics] def standardDeviationPopulation_[A](data: Array[A])(implicit num: Numeric[A]): Double = {
    math.sqrt(variancePopulation_(data))
  }


  private[statistics] def midRange_(data: Array[Int]): Double = {
    (max_(data) + min_(data)) / 2.0
  }

  private[statistics] def midRange_(data: Array[Double]): Double = {
    (max_(data) + min_(data)) / 2.0
  }

  private[statistics] def midRange_[A](data: Array[A])(implicit ord: Ordering[A], num: Numeric[A]): Double = {
    (num.toDouble(max_(data)) + num.toDouble(min_(data))) / 2.0
  }
}
