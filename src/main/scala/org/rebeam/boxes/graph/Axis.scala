package org.rebeam.boxes.graph

sealed trait Axis

object Axis {
  case object X extends Axis
  case object Y extends Axis

  def other(axis: Axis) = if (axis == X) Y else X
}
