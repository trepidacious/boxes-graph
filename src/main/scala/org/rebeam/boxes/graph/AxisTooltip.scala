package org.rebeam.boxes.graph

import org.rebeam.boxes.core._
import org.rebeam.boxes.swing.SwingView
import org.rebeam.boxes.swing.icons.IconFactory
import BoxScriptImports._
import BoxTypes._
import BoxUtils._

import java.awt.Color
import java.awt.geom.Rectangle2D
import java.text.DecimalFormat

import GraphMouseEventType._
import Axis._

object AxisTooltip {

  val format = new DecimalFormat("0.0000")
  val horizTabPainter = new GraphThreePartPainter(IconFactory.image("HorizontalLineLabel"))
  val vertTabPainter = new GraphThreePartPainterVertical(IconFactory.image("VerticalLineLabel"))
  val lineColor = SwingView.shadedBoxColor

  val boxThickness = 16
  val stringGap = 4
  val boxStart = 6
  val boxLength = 19

  def drawAxisLine(canvas: GraphCanvas, v: Double, a: Axis, label: String, color: Option[Color]) = {
    canvas.clipToData()
    val dataArea = canvas.spaces.dataArea
    val start = canvas.spaces.toPixel(dataArea.axisPosition(a, v))
    val end = start + canvas.spaces.pixelArea.axisPerpVec2(a)

    canvas.lineWidth = 1
    canvas.color = color.getOrElse(AxisTooltip.lineColor)
    canvas.line(start, end)

    canvas.color = GraphAxis.fontColor
    canvas.fontSize = GraphAxis.fontSize

    val size = canvas.stringSize(label)

    val colorOffset = if (color == None) 0 else 12;

    //TODO combine code
    a match {
      case X => {
        // AxisTooltip.vertTabPainter.paint(canvas, start + Vec2(-16, -4 - 23 - size.x - colorOffset), Vec2(16, size.x + 23 + colorOffset))

        canvas.color = SwingView.shadedBoxColor //new Color(0.2f, 0.2f, 0.2f, 0.8f)
        canvas.fillRect(start + Vec2(-boxThickness, -boxStart - boxLength - size.x - colorOffset), Vec2(boxThickness, size.x + boxLength + colorOffset))

        canvas.color = SwingView.selectedTextColor
        canvas.string(label, start + Vec2(-stringGap, -15 - colorOffset), Vec2(0, 0), -1)
        color.foreach(c => {
          val swatch = Area(start + Vec2(-11, -21), Vec2(7, 7))
          canvas.color = c
          canvas.fillRect(swatch)
          canvas.color = SwingView.selectedTextColor
          canvas.drawRect(swatch)
        })
      }
      case Y => {
        // AxisTooltip.horizTabPainter.paint(canvas, start + Vec2(4, -16), Vec2(size.x + 23 + colorOffset, 16))

        canvas.color = SwingView.shadedBoxColor //new Color(0.8f, 0.8f, 0.8f, 0.8f)
        canvas.fillRect(start + Vec2(boxStart, -boxThickness), Vec2(size.x + boxLength + colorOffset, boxThickness))

        canvas.color = SwingView.selectedTextColor
        canvas.string(label, start + Vec2(15 + colorOffset, -stringGap), Vec2(0, 0), 0)
        color.foreach(c => {
          val swatch = Area(start + Vec2(14, -11), Vec2(7, 7))
          canvas.color = c
          canvas.fillRect(swatch)
          canvas.color = SwingView.selectedTextColor
          canvas.drawRect(swatch)
        })
      }
    }

    size.x + 23 + 4 + colorOffset

  }

  def apply(axis: Axis, enabled: BoxR[Boolean]) = new AxisTooltip(axis, enabled)

}

class AxisTooltip(axis: Axis, enabled: BoxR[Boolean]) extends UnboundedGraphLayer {

  private val value = atomic { create(None: Option[Double]) }

  def paint = for {
    maybeV <- value()
    e <- enabled
  } yield {
    (canvas:GraphCanvas) => {
      if (e) {
        maybeV.foreach(v => {
          val label = AxisTooltip.format.format(v)
          AxisTooltip.drawAxisLine(canvas, v, axis, label, None)
        })
      }
    }
  }

  def onMouse(e: GraphMouseEvent) = for {
    en <- enabled
    _ <- if (en) {
      e.eventType match {
        case Move => {
          val axisPosition = e.spaces.pixelArea.axisRelativePosition(Axis.other(axis), e.spaces.toPixel(e.dataPoint)) * (if (axis == X) -1 else 1)
          if (axisPosition <= 0 && axisPosition > -32) {
            value() = Some(e.dataPoint.onAxis(axis))
          } else {
            value() = None
          }
        }
        case _ => (value() = None)
      }
    } else {
      nothing
    }
  } yield false
  
}
