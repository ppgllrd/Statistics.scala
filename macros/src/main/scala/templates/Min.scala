/** ****************************************************************************
  * Macro for computing minimum element in an array
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package templates

import scala.reflect.macros.blackbox

object Min {
  def min[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Expr[Array[A]])(ord: c.Expr[Ordering[A]]): c.Tree = {
    import c.universe._
    val actualType = c.weakTypeOf[A]

    @inline val lt = ord.tree match {
      case q"null" =>
        q""" (x:$actualType, y:$actualType) => x < y """
      case _ =>
        q""" $ord.lt """
    }

    q"""
      var m : $actualType = $data(0)
      var i = 1
      while(i < $data.length) {
        if($lt($data(i), m))
          m = $data(i)
        i += 1
      }
      m
    """
  }
}