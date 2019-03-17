/** ****************************************************************************
  * Macro for computing maximum element in an array
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package templates

import scala.reflect.macros.blackbox

object Max {
  def max[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Expr[Array[A]])(ord: c.Expr[Option[Ordering[A]]]): c.Tree = {
    import c.universe._
    val actualType = c.weakTypeOf[A]

    //  val b = actualType =:= typeOf[Int] || actualType =:= typeOf[Double]

    @inline val gt = ord.tree match {
      case q"scala.None" =>
        q""" (x:$actualType, y:$actualType) => x > y """
      case _ =>
        q""" ord.gt """
    }

    q"""

      var m : $actualType = $data(0)
      for(i <- 1 until $data.length)
        if($gt($data(i), m))
          m = $data(i)
      m
    """
  }
}