/** ****************************************************************************
  * Macro for multiplying elements in an array
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package templates

import scala.reflect.macros.blackbox

object Product {
  def product[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Tree): c.Tree = {
    import c.universe._
    val actualType = c.weakTypeOf[A]

    q"""
      var s : $actualType = 1
      for(x <- $data)
        s *= x
      s
    """
  }
}
