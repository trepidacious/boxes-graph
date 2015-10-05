package org.rebeam.boxes.graph

trait SeriesPainter{
  def paint(canvas: GraphCanvas, series: Series[_], shadow: Boolean = false)
  def linesDrawn(series: Series[_]): Boolean
}

class LineSeriesPainter extends SeriesPainter {
  def paint(canvas: GraphCanvas, s: Series[_], shadow: Boolean = false) {
    if (shadow && s.shadow) {
      canvas.color = GraphUtils.shadowColor
      canvas.lineWidth = s.width + 1
      canvas.path(s.curve.map(p => canvas.spaces.toPixel(p) + GraphUtils.shadowOffset))
    } else {
      canvas.color = s.color
      canvas.lineWidth = s.width
      canvas.dataPath(s.curve)
    }
  }
  def linesDrawn(series: Series[_]) = true
}

trait PointPainter {
  def paint(canvas: GraphCanvas, p:Vec2)
}

class CrossPointPainter extends PointPainter {
  def paint(canvas: GraphCanvas, p:Vec2) {
    canvas.line(p - Vec2(2, 2), p + Vec2(2, 2))
    canvas.line(p - Vec2(-2, 2), p + Vec2(-2, 2))
  }
}

class PlusPointPainter extends PointPainter {
  def paint(canvas: GraphCanvas, p:Vec2) {
    canvas.line(p - Vec2(0, 2), p + Vec2(0, 2))
    canvas.line(p - Vec2(2, 0), p + Vec2(2, 0))
  }
}

class SquarePointPainter extends PointPainter {
  def paint(canvas: GraphCanvas, p:Vec2) {
    canvas.drawRect(p - Vec2(2,2), Vec2(4, 4))
  }
}

object SeriesStyles {
  val cross = new PointSeriesPainter(new CrossPointPainter())
  val plus = new PointSeriesPainter(new PlusPointPainter())
  val square = new PointSeriesPainter(new SquarePointPainter())
  val line = new LineSeriesPainter()
}

class PointSeriesPainter(val pointPainter:PointPainter = new CrossPointPainter()) extends SeriesPainter {

  def paint(canvas: GraphCanvas, s: Series[_], shadow: Boolean = false) {
    if (shadow && s.shadow) {
      canvas.color = GraphUtils.shadowColor
      canvas.lineWidth = s.width + 1
      for (p <- s.curve.map(p => canvas.spaces.toPixel(p) + GraphUtils.shadowOffset)) {
        pointPainter.paint(canvas, p)
      }
    } else {
      canvas.color = s.color
      canvas.lineWidth = s.width
      for (p <- s.curve.map(p => canvas.spaces.toPixel(p))) {
        pointPainter.paint(canvas, p)
      }
    }
  }
  def linesDrawn(series: Series[_]) = false
}