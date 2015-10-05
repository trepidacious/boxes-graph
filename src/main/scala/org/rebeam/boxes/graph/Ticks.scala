package org.rebeam.boxes.graph

object Ticks {
  def apply(range: (Double, Double), pixels: Double, pixelsPerTick: Double) = {
    val absPixels = math.abs(pixels)

    val dataPerPixel = math.abs(range._2 - range._1) / absPixels
    val dataPerTick = pixelsPerTick * dataPerPixel

    //Find a power of ten value for data per tick
    val dataPerTickR = math.pow(10, math.round(math.log10(dataPerTick)).toInt)

    //See if we would be better using a multiple of the power of ten
    val dataPerTickBest = List(0.1, 0.5, 5, 10).foldLeft(dataPerTickR)((dataPerTickBest, scale) => {
      val dataPerTickScale = dataPerTickR * scale

      val pixelsPerTickScale = dataPerTickScale / dataPerPixel
      val pixelsPerTickBest = dataPerTickBest / dataPerPixel
      if (math.abs(pixelsPerTickScale - pixelsPerTick) < math.abs(pixelsPerTickBest - pixelsPerTick)) {
        dataPerTickScale
      } else {
        dataPerTickBest
      }
    })

    //Minor ticks are major ticks divided by 5.
    val dataPerMinorTick = dataPerTickBest / 5

    val firstTick = math.floor(range._1 / dataPerMinorTick).asInstanceOf[Int]
    val lastTick = math.ceil(range._2 / dataPerMinorTick).asInstanceOf[Int]

    Range.inclusive(firstTick, lastTick).map(x => (x * dataPerMinorTick, x % 5 == 0)).filter(xMajor => xMajor._1 >= range._1 && xMajor._1 <= range._2)
  }
}