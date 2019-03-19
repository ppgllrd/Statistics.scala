/** ****************************************************************************
  * Macro for adding and adding squares of elements in an array
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package templates

import scala.reflect.macros.blackbox

object Sums {
  def sum[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Expr[Array[A]])(num: c.Expr[Numeric[A]]): c.Tree = {

    import c.universe._
    val actualType = c.weakTypeOf[A]

    @inline val (plus, zero) = num.tree match {
      case q"null" =>
        ( q""" (x:$actualType, y:$actualType) => x + y """
        , q""" 0 """
        )
      case _ =>
        ( q""" num.plus """
        , q""" num.zero """
        )
    }

    q"""
      var s : $actualType = $zero
      for(i <- 0 until $data.length)
        s = $plus(s, $data(i))
      s
    """
  }

  def sumSqr[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Expr[Array[A]])(num: c.Expr[Numeric[A]]): c.Tree = {
    import c.universe._
    val actualType = c.weakTypeOf[A]

    @inline val (plusSqr, zero) = num.tree match {
      case q"null" =>
        ( q""" (x:$actualType, y:$actualType) => x + y*y """
        , q""" 0 """
        )
      case _ =>
        ( q""" (x:$actualType, y:$actualType) => num.plus(x, num.times(y,y)) """
        , q""" num.zero """
        )
    }
    q"""
      var s : $actualType = 0
      for(i <- 0 until $data.length)
        s = $plusSqr(s, $data(i))
      s
    """
  }
}
