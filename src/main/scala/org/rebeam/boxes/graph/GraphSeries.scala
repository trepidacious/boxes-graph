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

class GraphSeries[K](series: BoxR[List[Series[K]]], shadow: BoxR[Boolean] = just(false), enabled: BoxR[Boolean] = just(true)) extends GraphDisplayLayer {

  def paint = for {
    currentSeries <- series
    currentShadow <- shadow
    en <- enabled
  } yield {
    (canvas:GraphCanvas) => {
      if (en) {
        canvas.clipToData
        for (s <- currentSeries) {
          s.painter.paint(canvas, s, currentShadow)
        }
      }
    }
  }

  val viewRegion = atomic { cache {
    for {
      currentSeries <- series
      en <- enabled      
    } yield {
      val area = if (en) {
        currentSeries.foldLeft(None: Option[Area]){
          (seriesArea, series) => series.curve.foldLeft(seriesArea) {
            (area, v) => area.map(_.extendToContain(v)).orElse(Some(Area(v, Vec2.zero)))
          }
        }
      } else None
      RegionXY(area)
    }
  }}.r

}