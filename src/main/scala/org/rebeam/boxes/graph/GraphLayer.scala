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

trait GraphLayer {
  //When called, reads Box state and returns a method that will draw this state to a canvas
  def paint: BoxScript[(GraphCanvas) => Unit]

  //Handle an event, returns false to allow it to reach other layers, or true to consume it
  def onMouse(event: GraphMouseEvent): BoxScript[Boolean]
  
  def dataBounds: BoxScript[Option[Area]]
}

trait GraphDisplayLayer extends GraphLayer {
  def onMouse(event: GraphMouseEvent) = just(false)
}

abstract class UnboundedGraphDisplayLayer extends GraphDisplayLayer {
  val dataBounds = just(None:Option[Area])
}

abstract class UnboundedGraphLayer extends GraphLayer {
  val dataBounds = just(None:Option[Area])
}

abstract class UnboundedInputGraphLayer extends UnboundedGraphLayer {
  val paint = just((_:GraphCanvas) => ())
}