/** ****************************************************************************
  * Computation of mode of an array (most frequent element).
  * This implementation uses a HashMap. We could devise a
  * Divide and Conquer algorithm (see http://codinghelmet.com/exercises/array-mode)
  * but this implementation seems to perform better in practice if rate of
  * repetitions is not too high
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package statistics.descriptive.internals

import scala.collection.mutable

private[statistics] object mode {
  def apply[A](data: Array[A]): A = {
    val counters = mutable.HashMap[A, Int]().withDefaultValue(0)

    for (x <- data)
      counters(x) += 1

    var maxCounter = -1
    var mode: A = data(0)
    for ((x, counter) <- counters)
      if (counter > maxCounter) {
        mode = x
        maxCounter = counter
      }
    mode
  }
}
