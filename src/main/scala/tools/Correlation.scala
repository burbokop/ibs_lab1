package tools


object Correlation {
  def covariant(seq0: Seq[Double], seq1: Seq[Double]): Double =
    if (seq0.length == seq1.length && seq0.length > 1) {
      val aver0 = seq0.sum / seq0.length
      val aver1 = seq1.sum / seq1.length
        (for (i <- 0 until seq0.length) yield (seq0(i) - aver0) * (seq1(i) - aver1))
          .sum
    } else 0

  def coefficient(seq0: Seq[Double], seq1: Seq[Double]): Double =
    if (seq0.length == seq1.length && seq0.length > 1) {
      val aver0 = seq0.sum / seq0.length
      val aver1 = seq1.sum / seq1.length
      val max =
        (for (i <- 0 until seq0.length) yield Math.pow(seq0(i) - aver0, 2)).sum *
          (for (i <- 0 until seq0.length) yield Math.pow(seq1(i) - aver1, 2)).sum
      if (max != 0) {
        (for (i <- 0 until seq0.length) yield (seq0(i) - aver0) * (seq1(i) - aver1))
          .sum / Math.sqrt(max)
      } else 0
    } else 0


  def apply(seq0: Seq[Double], seq1: Seq[Double]): Seq[Double] =
    if (seq0.length == seq1.length && seq0.length > 1) {
      val halfSize: Int = Math.round(seq0.length / 2)
      for (i <- 0 until halfSize) yield
        covariant(seq0.slice(0, halfSize), seq1.slice(i, halfSize + i)) / (seq0.length - 1)
    } else Seq()

  def apply(seq: Seq[Double]): Seq[Double] = apply(seq, seq)


  def generateComparisonSequence(signalGenerator: SignalGenerator, nSeq: Seq[Int]): (Seq[Long], Seq[Long]) = {
    nSeq.map { N =>
      val x = (0 until N).map(_.toDouble)
      val y0 = signalGenerator.generateSeq(x)
      val y1 = signalGenerator.generateSeq(x)
      (
        ElapsedTimer.measure(() => Correlation(y0, y1))._1,
        ElapsedTimer.measure(() => Correlation(y0, y0))._1
      )
    }.unzip
  }

}
