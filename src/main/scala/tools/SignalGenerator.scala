package tools

import scala.util.Random

class SignalGenerator(harmonicsCount: Int, limitFreq: Double) {
  def generate(t: Double) =
    if(harmonicsCount != 0) {
      val delta = limitFreq / harmonicsCount
      val random = new Random()
      val amplitude = random.nextDouble()
      val phase = random.nextDouble()
      (0 until harmonicsCount).map[Double](
        i => amplitude * Math.sin((i + 1) * delta * t + phase)
      ).sum
    } else 0

  def generateSeq(seq: Seq[Double]) =
    seq.map[Double]({ i =>
      generate(i)
    })
}
