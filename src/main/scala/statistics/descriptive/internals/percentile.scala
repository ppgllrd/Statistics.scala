package statistics.descriptive.internals

private[statistics] object percentile {
  object partition {
    private def partition(data: Array[Double], lo: Int, hi: Int, pivot: Int): Int = {
      val pivotVal = data(pivot)

      // swap elements at hi and pivot
      data(pivot) = data(hi)
      data(hi) = pivotVal

      var p = lo
      for (i <- lo until hi) {
        if (data(i) < pivotVal) {
          // swap elements at i and p
          val t = data(i)
          data(i) = data(p)
          data(p) = t

          p += 1
        }
      }

      // move pivot to its final place
      data(hi) = data(p)
      data(p) = pivotVal

      p
    }

    private def select(data: Array[Double], m: Int): Double = {
      var lo = 0
      var hi = data.length - 1
      var stop = lo == hi
      while (!stop) {
        val pivot = partition(data, lo, hi, m) // third arg m is pivot but anyone would do

        if (pivot == m)
          stop = true
        else if (m < pivot)
          hi = pivot - 1
        else
          lo = pivot + 1
      }
      data(m)
    }

    // All these assume data in array can be shuffled, 0 < percentRank <= 100
    def nearestRank(data: Array[Double], percentRank: Double): Double = {
      val i = (percentRank * data.length / 100).ceil.toInt - 1
      select(data, i)
    }

    // Second variant in https://en.wikipedia.org/wiki/Percentile
    def linearInterpolation(data: Array[Double], percentRank: Double): Double = {
      if (percentRank == 0) {
        select(data, 0)
      } else if (percentRank == 100) {
        select(data, data.length - 1)
      } else {
        val rank = percentRank * (data.length - 1) / 100
        val intPart = rank.floor.toInt
        val fractPart = rank - intPart

        val dataIntPart = select(data, intPart)
        val dataIntPartNext = select(data, intPart + 1)

        dataIntPart + fractPart * (dataIntPartNext - dataIntPart)
      }
    }

  }


  object sorted {
    // All these assume data is sorted, 0 < percentRank <= 100

    def nearestRank(data: Array[Double], percentRank: Double): Double = {
      val i = (percentRank * data.length / 100).ceil.toInt - 1
      data(i)
    }

    // Second variant in https://en.wikipedia.org/wiki/Percentile
    def linearInterpolation(data: Array[Double], percentRank: Double): Double = {
      if(percentRank==0)
        data.head
      else if(percentRank==100)
        data.last
      else {
        val rank = percentRank * (data.length - 1) / 100
        val intPart = rank.floor.toInt
        val fractPart = rank - intPart
        data(intPart) + fractPart * (data(intPart + 1) - data(intPart))
      }
    }
  }
}
