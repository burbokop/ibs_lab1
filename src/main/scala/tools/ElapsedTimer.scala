package tools

class ElapsedTimer {
  val startNano = System.nanoTime()
  def elapsedNano() = System.nanoTime() - startNano
}
