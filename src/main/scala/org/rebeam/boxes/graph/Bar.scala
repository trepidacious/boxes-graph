package org.rebeam.boxes.graph

import java.awt.Color

case class Bar[K](key: K, value: Double, rangeMin: Option[Double] = None, rangeMax: Option[Double] = None, fill: Option[Color], outline: Option[Color] = Some(GraphUtils.barOutlineColor), width: Double = 1, painter: BarPainter = BarStyles.plain) {
  private val values = List(0, value, rangeMin.getOrElse(value), rangeMax.getOrElse(value))
  def min = values.reduceLeft(_ min _)
  def max = values.reduceLeft(_ max _)
  def interval = Vec2(min, max)
}
