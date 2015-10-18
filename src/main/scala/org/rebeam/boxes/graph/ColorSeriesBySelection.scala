package org.rebeam.boxes.graph

import Axis._
import GraphMouseEventType._
import org.rebeam.boxes.core._
import BoxScriptImports._
import BoxTypes._
import BoxUtils._

object ColorSeriesBySelection {
  def defaultSeriesToUnselected[K](s: Series[K]) = s.copy(color = GraphUtils.blendColors(s.color, GraphUtils.unselectedColor, 0.7), width = 1, shadow = false)
    
  def apply[K](series: BoxR[List[Series[K]]], indices: BoxR[Set[K]], seriesToUnselected: (Series[K] => Series[K]) = defaultSeriesToUnselected[K] _): BoxR[List[Series[K]]] = for {
    s <- series
    i <- indices
  } yield {
    val unselected = s.collect{
      case s: Series[K] if !i.contains(s.key) => seriesToUnselected(s)
    }
    val selected = s.filter(s => i.contains(s.key))

    unselected ::: selected    
  }
}
