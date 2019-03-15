/** ****************************************************************************
  * Macro for adding and adding squares of elements in an array
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package templates

import scala.reflect.macros.blackbox

object Sums {
  def sum[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Tree): c.Tree = {
    import c.universe._
    val actualType = c.weakTypeOf[A]

    q"""
      var s : $actualType = 0
      for(x <- $data)
        s += x
      s
    """
  }

  def sumSqr[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Tree): c.Tree = {
    import c.universe._
    val actualType = c.weakTypeOf[A]

    q"""
      var s : $actualType = 0
      for(x <- $data)
        s += x*x
      s
    """
  }
}
