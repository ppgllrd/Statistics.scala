/** ****************************************************************************
  * Macro for computing minimum element in an array
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package templates

import scala.reflect.macros.blackbox

object Min {
  def min[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Tree): c.Tree = {
    import c.universe._
    val actualType = c.weakTypeOf[A]

    q"""
      var m : $actualType = $data(0)
      for(i <- 1 until $data.length)
        if($data(i) < m)
          m = $data(i)
      m
    """
  }
}