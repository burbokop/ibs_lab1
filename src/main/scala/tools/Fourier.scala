package tools

import breeze.math.Complex
import breeze.math.Complex.powComplexComplexImpl
object Fourier {
  def transform(x: Int => Double, N: Int): Int => Double = (m: Int) =>
    (0 until N)
      .map(n => x(n) * Complex(Math.cos(2 * Math.PI * n * m / N), -Math.sin(2 * Math.PI * n * m / N)))
      .sum
      .abs
}
