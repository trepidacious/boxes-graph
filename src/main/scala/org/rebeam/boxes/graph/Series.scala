package org.rebeam.boxes.graph

import java.awt.Color

case class Series[K](key: K, curve: List[Vec2], color: Color = Color.black, width: Double = 1, shadow: Boolean = true, painter: SeriesPainter = SeriesStyles.line) {
  def pixelCurve(spaces: GraphSpaces) = curve.map(p => spaces.toPixel(p))
}
