/** ****************************************************************************
  * Macro for computing minimum element in an array
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package templates

import scala.reflect.macros.blackbox

object Min {
  def min[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Expr[Array[A]])(ord: c.Expr[Option[Ordering[A]]]): c.Tree = {
    import c.universe._
    val actualType = c.weakTypeOf[A]

    //  val b = actualType =:= typeOf[Int] || actualType =:= typeOf[Double]

    @inline val lt = ord.tree match {
      case q"scala.None" =>
        q""" (x:$actualType, y:$actualType) => x < y """
      case _ =>
        q""" ord.lt """

    }

    q"""

      var m : $actualType = $data(0)
      for(i <- 1 until $data.length)
        if($lt($data(i), m))
          m = $data(i)
      m
    """
  }
}