package tools

class ElapsedTimer {
  val startNano = System.nanoTime()
  def elapsedNano() = System.nanoTime() - startNano

}

object ElapsedTimer {
  def measure[T](f: () => T): (Long, T) = {
    val timer = new ElapsedTimer()
    val result = f()
    (timer.elapsedNano, result)
  }
}