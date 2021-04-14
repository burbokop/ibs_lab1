import Main.x
import tools.{Correlation, Fourier, SignalGenerator}

import org.nspl.{line, _}
import awtrenderer._

import scala.language.postfixOps

object Main extends App {
  val signalGenerator = new SignalGenerator(10, 900)

  val x = (0 until 256).map(_.toDouble)
  val y0 = signalGenerator.generateSeq(x)
  val y1 = signalGenerator.generateSeq(x)
  println(s"y0: $y0")
  println(s"y1: $y1")

  val k00 = Correlation.coefficient(y0, y0)
  val k01 = Correlation.coefficient(y0, y1)
  println(s"corelation y0 with y0: $k00")
  println(s"corelation y0 with y1: $k01")

  val c00 = Correlation(y0, y0)
  val c01 = Correlation(y0, y1)
  println(s"corelation func y0 with y0: $c00")
  println(s"corelation func y0 with y1: $c01")

  val fourierY0Func = Fourier.transform(y0, y0.length)
  val fourierY0Seq = (0 until y0.length).map(fourierY0Func)

  println(s"fourier transform of y0: $fourierY0Seq")

  val plot0 = xyplot(
    x -> y0 -> line(stroke = StrokeConf(0.1 fts), color = Color(255, 0, 0)),
    x -> c00.map(_ * 10) -> line(stroke = StrokeConf(0.1 fts), color = Color(0, 0, 255)),
  )(
    ylab = "y",
    xlab = "x",
    main = "red - original signal, blue - auto correlation",
  )

  val plot1 = xyplot(
    x -> y0 -> line(stroke = StrokeConf(0.1 fts), color = Color(255, 0, 0)),
    x -> y1 -> line(stroke = StrokeConf(0.1 fts), color = Color(0, 255, 0)),
    x -> c01.map(_ * 10) -> line(stroke = StrokeConf(0.1 fts), color = Color(0, 0, 255))
  )(
    ylab = "y",
    xlab = "x",
    main = "red - orig sig 0, green - orig sig 1, blue - correlation sig0 with sig1",
  )

  val plot2 = xyplot(
    x -> fourierY0Seq -> line(stroke = StrokeConf(0.1 fts), color = Color(0, 0, 255))
  )(
    ylab = "A",
    xlab = "freq",
    main = "blue - fourier transform of sig0",
  )


  show(plot0)
  show(plot1)
  show(plot2)
}
