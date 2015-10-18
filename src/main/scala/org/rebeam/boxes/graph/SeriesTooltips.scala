package org.rebeam.boxes.graph

import org.rebeam.boxes.core._
import org.rebeam.boxes.swing.SwingView
import org.rebeam.boxes.swing.icons.IconFactory
import BoxScriptImports._
import BoxTypes._
import BoxUtils._

import java.awt.geom.Line2D

import GraphMouseEventType._
import Axis._

trait SeriesTooltipRenderer[K] {
  def paint(series: Series[K], pixelPos: Vec2): BoxScript[(GraphCanvas) => Unit]
}

trait TooltipPrinter[K] {
  def print(k: K): BoxScript[String]
}

class StringTooltipPrinter[K] extends TooltipPrinter[K] {
  def print(k: K) = just(k.toString())
}

class StringSeriesTooltipRenderer[K](printer: TooltipPrinter[K]) extends SeriesTooltipRenderer[K]{
  def paint(series: Series[K], pixelPos: Vec2) = for {
    s <- printer.print(series.key)
  } yield (canvas: GraphCanvas) => canvas.drawTooltip(s, pixelPos)
}

class HighlightSeriesTooltipRenderer[K] extends SeriesTooltipRenderer[K]{
  def paint(series: Series[K], pixelPos: Vec2) = just((canvas: GraphCanvas) => {
    canvas.clipToData()
    series.painter.paint(canvas, series.copy(width = series.width + 3))
    canvas.clipToAll()
  })
}

object SeriesTooltips {
  val maxRadius = 10

  def string[K](series: BoxR[List[Series[K]]], enabled: BoxR[Boolean], printer: TooltipPrinter[K]) = 
    new SeriesTooltips[K](enabled, series, new StringSeriesTooltipRenderer[K](printer))

  def highlight[K](series: BoxR[List[Series[K]]], enabled: BoxR[Boolean]) = 
    new SeriesTooltips[K](enabled, series, new HighlightSeriesTooltipRenderer[K]())
}

class SeriesTooltips[K](enabled: BoxR[Boolean], series: BoxR[List[Series[K]]], renderer: SeriesTooltipRenderer[K]) extends UnboundedGraphLayer {

  private val toPaint = atomic { create(None: Option[(Series[K], Vec2)]) }

  def paint: BoxScript[(GraphCanvas) => Unit] = for {
    en <- enabled
    tp <- toPaint()
    p <- tp.map(pair => renderer.paint(pair._1, pair._2)).getOrElse(GraphLayer.drawNothing)
  } yield p

  def onMouse(e: GraphMouseEvent) = for {
    en <- enabled
    s <- series
    _ <- toPaint() = e.eventType match {
      case Move if en => SeriesSelection.selectedSeries(s, e).map((_, e.spaces.toPixel(e.dataPoint)))  //Stick the pixel point in with selected series
      case _ => None
    }
  } yield false

}