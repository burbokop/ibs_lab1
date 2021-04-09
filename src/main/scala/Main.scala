import tools.{Correlation, SignalGenerator}

import java.awt.image.BufferedImage
import scala.util.Random


object Main extends App {
  val signalGenerator = new SignalGenerator(10, 900)

  val x = (0 until 256).map(_.toDouble)
  val y = signalGenerator.generateSeq(x)

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
