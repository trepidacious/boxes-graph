package org.rebeam.boxes.graph

sealed trait GraphMouseButton
object GraphMouseButton {
  case object LeftButton extends GraphMouseButton
  case object MiddleButton extends GraphMouseButton
  case object RightButton extends GraphMouseButton
  case object NoButton extends GraphMouseButton
}

sealed trait GraphMouseEventType
object GraphMouseEventType {
  case object Press extends GraphMouseEventType
  case object Release extends GraphMouseEventType
  case object Drag extends GraphMouseEventType
  case object Move extends GraphMouseEventType
  case object Click extends GraphMouseEventType
  case object Enter extends GraphMouseEventType
  case object Exit extends GraphMouseEventType
  case object Consumed extends GraphMouseEventType
}

case class GraphMouseEvent (spaces: GraphSpaces, dataPoint: Vec2, eventType: GraphMouseEventType, button: GraphMouseButton)
