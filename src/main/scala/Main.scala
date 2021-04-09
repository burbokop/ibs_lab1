import java.awt.image.BufferedImage
import scala.util.Random


object Main extends App {

  def signal(t: Double, n: Int, limitFreq: Double) = {
    if(n != 0) {
      val delta = limitFreq / n
      val random = new Random()
      val amplitude = random.nextDouble()
      val phase = random.nextDouble()
      (0 until n).map[Double](
        i => amplitude * Math.sin((i + 1) * delta * t + phase)
      ).sum
    } else 0
  }

  def calc(n: Int, limitFreq: Double, seq: Seq[Double]) = {
    seq.map[Double]({ i =>
      signal(i, n, limitFreq)
    }).toList
  }


  val x = (0 until 256).map(_.toDouble)
  val y = calc(10, 900, x)

  println(y)

  import org.nspl._
  import awtrenderer._
  val plot = xyplot(x -> y -> line())(
    ylab = "y",
    xlab = "x",
    main = "main"
  )

  show(plot)
}
