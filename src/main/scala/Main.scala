
import tools.{Correlation, ElapsedTimer, Fourier, SignalGenerator}
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


  //порівняйте швидкість виконання кореляції та автокореляції для двох сигналів різної довжини
  //(1000...5000 із кроком 200) та зобразіть їх у вигляді кривих на графіку залежності часу виконання від кількості дискретних відліків.
  val correlationNSeq = 1000 to 5000 by 200
  val (corelTime, autocorelTime) = Correlation.generateComparisonSequence(signalGenerator, correlationNSeq)
  // --------

  val fourierTimer = new ElapsedTimer()
  val fourierY0Func = Fourier.transform(y0, y0.length)
  val fourierY0Seq = (0 until y0.length).map(fourierY0Func)
  val fourierTimerElapsed = fourierTimer.elapsedNano

  val fastFourierTimer = new ElapsedTimer()
  val fastFourierY0Func = Fourier.fastTransform(y0, y0.length)
  val fastFourierY0Seq = (0 until y0.length).map(fastFourierY0Func)
  val fastFourierTimerElapsed = fastFourierTimer.elapsedNano

  println(s"slow fourier transform duration: $fourierTimerElapsed us")
  println(s"fast fourier transform duration: $fastFourierTimerElapsed us")

  val cpNSeq = 32 to 4096 by 16 //TO DO made 4096
  val cpSeq = Fourier.generateComparisonSequence(signalGenerator, cpNSeq)

  println(s"fourier transform of y0: $fourierY0Seq")
  println(s"fast fourier transform of y0: $fastFourierY0Seq")
  println(s"fourier transform time comparison sequence in ms: $cpSeq")

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
    correlationNSeq.map(_.toDouble) -> corelTime.map(_.toDouble) -> line(stroke = StrokeConf(0.1 fts), color = Color(255, 0, 0)),
    correlationNSeq.map(_.toDouble) -> autocorelTime.map(_.toDouble) -> line(stroke = StrokeConf(0.1 fts), color = Color(0, 255, 0)),
  )(
    ylab = "Nanoseconds",
    xlab = "N",
    main = "red - corelation time, auto corel time",
  )

  val plot3 = xyplot(
    x -> fourierY0Seq -> line(stroke = StrokeConf(0.1 fts), color = Color(0, 0, 255)),
    x -> fastFourierY0Seq -> line(stroke = StrokeConf(0.1 fts), color = Color(255, 0, 255))
  )(
    ylab = "A",
    xlab = "freq",
    main = "blue - fourier transform of sig0, violet - fast fourier transform of sig0",
  )

  val plot4 = xyplot(
    cpNSeq.map(_.toDouble) -> cpSeq.map(_._1) -> line(stroke = StrokeConf(0.1 fts), color = Color(0, 0, 255)),
    cpNSeq.map(_.toDouble) -> cpSeq.map(_._2) -> line(stroke = StrokeConf(0.1 fts), color = Color(255, 0, 255))
  )(
    ylab = "Duration",
    xlab = "N",
    main = "blue - fourier transform elapsed time in ms, violet - fast fourier transform elapsed time in ms",
  )



  show(plot0)
  show(plot1)
  show(plot2)
  show(plot3)
  show(plot4)
}
