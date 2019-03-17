/** ****************************************************************************
  * Implementation of different descriptive statistics .
  * These are private and assumed to avoid checking redundant
  * preconditions.
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package statistics.descriptive.internals

import scala.language.experimental.macros

private[statistics] object selection {
  def selectionMacro[A](data: Array[A], k: Int)(ord: Option[Ordering[A]]): A =
    macro templates.Selection.selection[A]

  def selection_(data: Array[Int], k: Int): Int =
    selectionMacro(data, k)(None)

  def selection_(data: Array[Double], k: Int): Double =
    selectionMacro(data, k)(None)

  def selection_[A](data: Array[A], k: Int)(implicit ord: Ordering[A]): A =
    selectionMacro(data, k)(Some(ord))
}
