package tools

object Correlation {
  def coefficient(seq0: Seq[Double], seq1: Seq[Double]): Double =
    if (seq0 == seq1 && seq0.length > 1) {
      val aver0 = seq0.sum / seq0.length
      val aver1 = seq1.sum / seq1.length
      (for (i <- 0 until seq0.length) yield (seq0(i) - aver0) * (seq1(i) - aver1))
        .sum / (seq0.length - 1)
    } else 0


  def apply(seq0: Seq[Double], seq1: Seq[Double]): Seq[Double] =
    if (seq0 == seq1 && seq0.length > 1) {
      val size: Int = Math.round(seq0.length / 2)
      for (i <- 0 until size) yield
        coefficient(seq0.slice(0, size), seq1.slice(i, size + i))
    } else Seq()

  def apply(seq: Seq[Double]): Seq[Double] = apply(seq, seq)

}
