package tools

import breeze.math.Complex
object Fourier {
  def transform(x: Int => Double, N: Int): Int => Double = (m: Int) =>
    (0 until N)
      .map(n => x(n) * Complex(Math.cos(2 * Math.PI * n * m / N), -Math.sin(2 * Math.PI * n * m / N)))
      .sum
      .abs

  def fastTransform(x: Int => Double, N: Int): Int => Double = (m: Int) =>
    if (N > 1) {
      val sc = 2 * Math.PI * m / N
      case class EulerCoefficient(n: Int, value: Complex)
      val coefficientSeq = (0 until N / 2 - 1).toArray.map(n => EulerCoefficient(n, Complex(Math.cos(sc * (2 * n)), -Math.sin(sc * (2 * n)))))
      (
        coefficientSeq.map(c => x(2 * c.n) * c.value).sum +
          Complex(0, -Math.sin(sc)) *
            coefficientSeq.map(c => x(2 * c.n + 1) * c.value).sum
        ).abs
    } else 0


  def generateComparisonSequence(signalGenerator: SignalGenerator, nSeq: Seq[Int]) = {
    nSeq.map { N =>
      println(s"Compare DFT & FFT N: $N, MAX: ${nSeq.last}")
      val signal = signalGenerator.generateSeq((0 until N).map(_.toDouble))
      type F = (Int => Double, Int) => (Int => Double)
      def measure(fourier: F) = {
        val timer = new ElapsedTimer()
        val f = fourier(signal, signal.length)
        val result = (0 until signal.length).map(f)
        (timer.elapsedNano, result)
      }
      val m0 = measure(Fourier.transform)
      val m1 = measure(Fourier.fastTransform)
      (m0._1 * 0.001 * 0.001, m1._1 * 0.001 * 0.001)
    }
  }
}
