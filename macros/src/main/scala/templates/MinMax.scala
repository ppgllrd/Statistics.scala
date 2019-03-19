package templates

import scala.reflect.macros.blackbox

object MinMax {
  def minMax[A: c.WeakTypeTag](c: blackbox.Context)(data: c.Expr[Array[A]])(ord: c.Expr[Ordering[A]]): c.Tree = {
    import c.universe._
    val actualType = c.weakTypeOf[A]

    @inline val lt = ord.tree match {
      case q"null" =>
        q""" (x:$actualType, y:$actualType) => x < y """
      case _ =>
        q""" $ord.lt """
    }

    q"""
      var (mini, maxi, i) =
        if($data.length % 2 == 0) {
          if($lt($data(0), $data(1)))
            ($data(0), $data(1), 2)
          else
            ($data(1), $data(0), 2)
        } else
          ($data(0), $data(0), 1)

      while(i < $data.length) {
        if($lt($data(i), $data(i+1))) {
          if($lt($data(i), mini))
            mini = $data(i)
          if($lt(maxi, $data(i+1)))
            maxi = $data(i+1)
        } else {
          if($lt($data(i+1), mini))
            mini = $data(i+1)
          if($lt(maxi, $data(i)))
            maxi = $data(i)
        }
        i += 2
      }
      (mini, maxi)
    """
  }
}