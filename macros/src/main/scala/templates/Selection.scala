/** ****************************************************************************
  * Selection algorithm
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package templates

import scala.reflect.macros.blackbox

object Selection {
  // Adapted from Numerical R ecipes book
  def selection[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Expr[Array[A]], k: c.Expr[Int])(ord: c.Expr[Option[Ordering[A]]]): c.Tree = {
    import c.universe._
    val actualType = c.weakTypeOf[A]

    @inline val gt = ord.tree match {
      case q"scala.None" =>
        q""" (x:$actualType, y:$actualType) => x > y """
      case _ =>
        q""" ord.gt """
    }

    q"""
    @inline def swap(i: Int, j: Int): Unit = {
      val temp = $data(i)
      $data(i) = $data(j)
      $data(j) = temp
    }

    var left = 0
    var right = $data.length - 1

    var found = false
    while (!found) {
      val leftPlus1 = left + 1

      if (right <= leftPlus1) {
        // 1 or 2 elements
        if (right == leftPlus1 && $gt($data(left), $data(right))) {
          // 2 elements
          swap(left, right)
        }
        found = true
      } else {
        val mid = (left + right) / 2

        // Set median of left, mid, and right elements as pivot.
        // Force data(left) ≤ data(leftPlus1) and data(right) ≥ data(leftPlus1)
        swap(mid, leftPlus1)
        if ($gt($data(left), $data(right)))
          swap(left, right)

        if ($gt($data(leftPlus1), $data(right)))
          swap(leftPlus1, right)

        if ($gt(data(left), $data(leftPlus1)))
          swap(left, leftPlus1)

        var i = leftPlus1
        var j = right
        val pivot = $data(leftPlus1)

        var partitioned = false
        do {
          do i += 1 while ($gt(pivot, $data(i)))
          do j -= 1 while ($gt($data(j), pivot))
          if (i <= j)
            swap(i, j)
          else
            partitioned = true
        } while (!partitioned)

        $data(leftPlus1) = $data(j)
        $data(j) = pivot

        if (j >= $k)
          right = j - 1
        if (j <= $k)
          left = i
      }
    }
    $data($k)
    """
  }
}
