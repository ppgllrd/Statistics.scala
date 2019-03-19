/** ****************************************************************************
  * Bootstrapping algorithms.
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package statistics.descriptive.internals

private[statistics] object bootstrapping {
  def meanConfidenceInterval(data: Array[Double], alpha: Double = 0.9, numSamples: Int = 1000): (Double, Double) = {
    val size = data.length

    // original data mean
    val mean = statistics.descriptive.internals.mean_(data)

    val sample = new Array[Double](size)
    val sampleMeanDiffs = new Array[Double](numSamples)

    val rnd = new scala.util.Random()
    for (i <- 0 until numSamples) {
      // take a with-replacement sample of SAME SIZE
      for (j <- 0 until size)
        sample(j) = data(rnd.nextInt(size))
      // compute difference between means of sample and original data
      sampleMeanDiffs(i) = statistics.descriptive.internals.mean_(sample) - mean
    }

    val lowRank = 100 * (1 - alpha) / 2
    val highRank = 100 * (1 + alpha) / 2

    val lowPercentile = statistics.descriptive.internals.percentile.linearInterpolation_(sampleMeanDiffs, lowRank)
    val highPercentile = statistics.descriptive.internals.percentile.linearInterpolation_(sampleMeanDiffs, highRank)

    (mean + lowPercentile, mean + highPercentile)
  }
}
