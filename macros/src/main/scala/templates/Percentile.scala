/** ****************************************************************************
  * Percentile algorithms
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package templates

import scala.reflect.macros.blackbox

object Percentile {
  // Second variant in https://en.wikipedia.org/wiki/Percentile
  def linearInterpolation[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Expr[Array[A]], percentRank: c.Expr[Double])(ord: c.Expr[Ordering[A]], num: c.Expr[Numeric[A]]): c.Tree = {
    import c.universe._

    q"""
      if (percentRank == 0) {
        $num.toDouble(statistics.descriptive.internals.selection.selection_($data, 0))
      } else if (percentRank == 100) {
        $num.toDouble(statistics.descriptive.internals.selection.selection_($data, $data.length - 1))
      } else {
        val rank = percentRank * ($data.length - 1) / 100
        val intPart = rank.floor.toInt
        val fractPart = rank - intPart

        val dataIntPart = $num.toDouble(statistics.descriptive.internals.selection.selection_($data, intPart))
        val dataIntPartNext = $num.toDouble(statistics.descriptive.internals.selection.selection_($data, intPart + 1))

        dataIntPart + fractPart * (dataIntPartNext - dataIntPart)
      }
  """
  }

  def percentile[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Expr[Array[A]], percentRank: c.Expr[Double], canReshuffle: c.Expr[Boolean])(ord: c.Expr[Ordering[A]], num: c.Expr[Numeric[A]]): c.Tree = {
    import c.universe._

    q"""
      assert($data.nonEmpty, "percentile: data must be non-empty")
      assert(0 <= $percentRank && $percentRank <= 100, "percentile: percentRank must be in [0,100]")
      if($canReshuffle)
        statistics.descriptive.internals.percentile.linearInterpolation_($data, $percentRank)
      else {
        val copy = $data.clone()
        statistics.descriptive.internals.percentile.linearInterpolation_(copy, $percentRank)
      }
    """
  }

  def median[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Expr[Array[A]], canReshuffle: c.Expr[Boolean])(ord: c.Expr[Ordering[A]], num: c.Expr[Numeric[A]]): c.Tree = {
    import c.universe._

    q"""
      assert($data.nonEmpty, "median: data must be non-empty")
      if($canReshuffle)
        statistics.descriptive.internals.percentile.linearInterpolation_($data, 50)
      else {
        val copy = $data.clone()
        statistics.descriptive.internals.percentile.linearInterpolation_(copy, 50)
      }
    """
  }
}
