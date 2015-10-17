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

class GraphSeries[K](series: BoxR[List[Series[K]]], shadow: BoxR[Boolean] = just(true)) extends GraphDisplayLayer {

  def paint = for {
    currentSeries <- series
    currentShadow <- shadow
  } yield {
    (canvas:GraphCanvas) => {
      canvas.clipToData
      for (s <- currentSeries) {
        s.painter.paint(canvas, s, currentShadow)
      }
    }
  }

  val viewArea = for {
    currentSeries <- series
  } yield {
    currentSeries.foldLeft(None: Option[Area]){(seriesArea, series) => series.curve.foldLeft(seriesArea){
      (area, v) => area match {
        case None => Some(Area(v, Vec2.zero))
        case Some(a) => Some(a.extendToContain(v))
      }
    }}
  }

  val viewRegion = viewArea.map(RegionXY(_))
}