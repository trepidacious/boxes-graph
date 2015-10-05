package org.rebeam.boxes.graph

import java.awt.Color

trait BarPainter{
  def paint(canvas: GraphCanvas, 
      x: Double, barWidth: Double,
      bar: Bar[_],
      shadow:Boolean = false)
}

class PlainBarPainter extends BarPainter {
  def paint(canvas: GraphCanvas,
      x: Double, barWidth: Double,
      bar: Bar[_],
      shadow:Boolean = false) {
    
    val area = Area(Vec2(x, 0), Vec2(barWidth, bar.value))
    
    //Round before drawing, so that inset works exactly on pixels.
    val pixelArea = canvas.spaces.toPixel(area).round
    val midEnd = pixelArea.fromUnit(Vec2(0.5, 1)).round
    
    if (shadow) {
      canvas.color = GraphUtils.barShadowColor
      canvas.fillRect(pixelArea.translate(GraphUtils.barShadowOffset))
    } else {
      bar.fill.foreach((c:Color) => {
        canvas.color = c
        canvas.fillRect(pixelArea)
        canvas.color = new Color(1f, 1f, 1f, 0.4f)
        canvas.drawRect(pixelArea.inset(1, 1, 1, 1))
      })
      bar.outline.foreach((c:Color) => {
      canvas.lineWidth = bar.width
        canvas.color = c
        canvas.drawRect(pixelArea)
      })
      def drawRange(range: Option[Double], halo: Boolean) = for {c <- bar.outline; r <- range}{
        val lineEnd = Vec2(midEnd.x, canvas.spaces.toPixel(Vec2(0, r)).y).round
        if (halo) {
          canvas.color = new Color(1f, 1f, 1f, 0.8f)
          canvas.lineWidth = bar.width + 2
        } else {
          canvas.color = c
          canvas.lineWidth = bar.width          
        }
        canvas.line(midEnd, lineEnd)
        canvas.line(lineEnd - Vec2(4, 0), lineEnd + Vec2(4, 0))
      }
      drawRange(bar.rangeMin, true)
      drawRange(bar.rangeMax, true)
      drawRange(bar.rangeMin, false)
      drawRange(bar.rangeMax, false)
    }
    
    //TODO draw the range bars
  }
}

object BarStyles {
  val plain = new PlainBarPainter()
}