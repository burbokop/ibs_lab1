package tools

import scala.util.Random

class SignalGenerator(harmonicsCount: Int, limitFreq: Double, amplitude: Double = 1) {
  def generate(t: Double) =
    if(harmonicsCount != 0) {
      val delta = limitFreq / harmonicsCount
      val random = new Random()
      val amplitudeRandom = random.nextDouble()
      val phase = random.nextDouble()
      (0 until harmonicsCount).map[Double](
        i => amplitude * amplitudeRandom * Math.sin((i + 1) * delta * t + phase)
      ).sum
    } else 0

  def generateSeq(seq: Seq[Double]) =
    seq.map[Double]({ i =>
      generate(i)
    })
}

object SignalGenerator {
  def deltaSeq(seq0: Seq[Double], seq1: Seq[Double]): Seq[Double] =
    (0 until Math.min(seq0.length, seq1.length))
      .map(i => Math.abs(seq0(i) - seq1(i)))
}