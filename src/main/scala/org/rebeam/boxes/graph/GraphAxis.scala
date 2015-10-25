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

object GraphAxis {
  val fontSize = 10
  val titleFontSize = 12
  val fontColor = SwingView.textColor
  val axisColor = SwingView.dividingColor
  val axisHighlightColor = SwingView.alternateBackgroundColor.brighter
  val gridMajorColor = new Color(0f, 0f, 0f, 0.08f)
  val gridMinorColor = new Color(0f, 0f, 0f, 0.03f)
  val defaultFormat = new DecimalFormat("0.###")

  def apply(axis: Axis, pixelsPerMajor: Int = 100, format: DecimalFormat = GraphAxis.defaultFormat) = new GraphAxis(axis, pixelsPerMajor, format)
}

class GraphAxis(val axis: Axis, val pixelsPerMajor: Int = 100, val format: DecimalFormat = GraphAxis.defaultFormat, val gridlines: Boolean = true, val minorGridLines: Boolean = false, highlights: Boolean = false) extends UnboundedGraphDisplayLayer {

  def paint = just (
    (canvas:GraphCanvas) => {
      val dataArea = canvas.spaces.dataArea

      val ticks = Ticks(dataArea.axisBounds(axis), canvas.spaces.pixelArea.axisSize(axis), pixelsPerMajor)

      ticks.foreach(t => {
        val (p, major) = t
        val start = canvas.spaces.toPixel(dataArea.axisPosition(axis, p))

        canvas.color = GraphAxis.axisColor
        axis match {
          case X => canvas.line(start, start + Vec2(0, if (major) 8 else 4))
          case Y => canvas.line(start, start + Vec2(if (major) -8 else -4, 0))
        }
        if (highlights) {
          canvas.color = GraphAxis.axisHighlightColor
          axis match {
            case X => canvas.line(start + Vec2(1, 0), start + Vec2(1, if (major) 8 else 4))
            case Y => canvas.line(start + Vec2(0, 1), start + Vec2(if (major) -8 else -4, 1))
          }
        }

        if (major) {
          canvas.color = GraphAxis.fontColor
          canvas.fontSize = GraphAxis.fontSize
          axis match {
            case X => canvas.string(format.format(p), start + Vec2(0, 10), Vec2(0.5, 1))
            case Y => canvas.string(format.format(p), start + Vec2(-10, 0), Vec2(1, 0.5))
          }
          if (gridlines) {
            canvas.color = GraphAxis.gridMajorColor
            canvas.line(start, start + canvas.spaces.pixelArea.axisPerpVec2(axis))
          }
        } else if (minorGridLines) {
          canvas.color = GraphAxis.gridMinorColor
          canvas.line(start, start + canvas.spaces.pixelArea.axisPerpVec2(axis))
        }
      })
    }
  )
}