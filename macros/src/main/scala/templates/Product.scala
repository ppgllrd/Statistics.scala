/** ****************************************************************************
  * Macro for multiplying elements in an array
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package templates

import scala.reflect.macros.blackbox

object Product {
  def product[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Expr[Array[A]])(num: c.Expr[Option[Numeric[A]]]): c.Tree = {

    import c.universe._
    val actualType = c.weakTypeOf[A]

    @inline val (times, one) = num.tree match {
      case q"scala.None" =>
        ( q""" (x:$actualType, y:$actualType) => x * y """
        , q""" 1 """
        )
      case _ =>
        ( q""" num.times """
        , q""" num.one """
        )
    }

    q"""
      var s : $actualType = $one
      for(x <- $data)
        s = $times(s, x)
      s
    """
  }
}
