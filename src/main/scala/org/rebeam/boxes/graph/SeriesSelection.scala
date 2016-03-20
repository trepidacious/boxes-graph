package org.rebeam.boxes.graph

import java.awt.geom.Line2D
import java.awt.Color

object SeriesSelection {

  def curveNearestPoint(curve: List[Vec2], p: Vec2) = {
    val squared = curve.foldLeft(Double.PositiveInfinity) {
      (previousSquaredLength, curvePoint) => math.min(previousSquaredLength, (p - curvePoint).squaredLength)
    }

    math.sqrt(squared)
  }
  
  def curveNearestApproach(curve: List[Vec2], p: Vec2) = {
    val squared = curve.foldLeft((Double.PositiveInfinity, None:Option[Vec2])){
      (result, current) => {
        val previousDistance = result._1
        val maybePrevious = result._2
        maybePrevious match {
          case None => (previousDistance, Some(current))
          case Some(previous) => {
            val distance = Line2D.ptSegDistSq(previous.x, previous.y, current.x, current.y, p.x, p.y)
            (math.min(previousDistance, distance), Some(current))
          }
        }
      }
    }
    math.sqrt(squared._1)
  }
  
  def seriesDistance(spaces: GraphSpaces, series: Series[_], p: Vec2) = {
    //Use the curve in PIXEL coords, to make sense to user
    if (series.painter.linesDrawn(series)) {
      curveNearestApproach(series.pixelCurve(spaces), p)
    } else {
      curveNearestPoint(series.pixelCurve(spaces), p)
    }
  }
  
  def selectedSeriesIndex[K](currentSeries: List[Series[K]], e: GraphMouseEvent): Option[Int] = {
    val pixelPoint = e.spaces.toPixel(e.dataPoint)
    val dataPoint = e.dataPoint
    
    if (e.spaces.dataArea.contains(dataPoint) && currentSeries.size > 0) {
      val distances = currentSeries.map(s => SeriesSelection.seriesDistance(e.spaces, s, pixelPoint)).zipWithIndex
      val index = distances.minBy(pair => pair._1)._2
      val d = distances(index)._1

      if (d < SeriesTooltips.maxRadius) {
        Some(index)
      } else {
        None
      }
    } else {
      None
    }
  }
    
  def selectedSeries[K](currentSeries: List[Series[K]], e:GraphMouseEvent): Option[Series[K]] = selectedSeriesIndex(currentSeries, e).map(currentSeries(_))
}