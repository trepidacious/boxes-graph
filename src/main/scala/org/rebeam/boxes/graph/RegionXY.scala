package org.rebeam.boxes.graph

import Axis._

/**
 * Represents a (possibly unbounded) region of Vec2 values
 * This is the product of two Intervals, one for x axis and one for y - each Interval function
 * has a corresponding RegionXY function that just uses the underlying intervals appropriately.
 * This includes rectangles (when we have two finite intervals), horizontal and vertical lines
 * (when we have one Interval containing all values, and one containing only one value), and
 * a variety of infinite regions like x >= v1 and/or y <= v2 for some v1, v2.
 */
case class RegionXY(x: Interval, y: Interval) {

  def normalised = RegionXY(x.normalised, y.normalised)

  def contains(v: Vec2): Boolean = x.contains(v.x) && y.contains(v.y)

  def definedOuter(i: RegionXY) = RegionXY(x.definedOuter(i.x), y.definedOuter(i.y))

  def isFinite = x.isFinite && y.isFinite

  /**
   * Converts to an Option[Area] - this is Some(area) if finite, or None if infinite
   * Note that if either x or y Interval is denormalised, the Area will be too.
   */
  def toArea: Option[Area] = (x.min, x.max, y.min, y.max) match {
    case (Some(xmin), Some(xmax), Some(ymin), Some(ymax)) => Some(Area(Vec2(xmin, ymin), Vec2(xmax - xmin, ymax - ymin)))
    case _ => None
  }
  
  def toArea(sizeX: Double, sizeY: Double): Area = {
    val xf = x.toFiniteInterval(sizeX)
    val yf = y.toFiniteInterval(sizeY)
    Area(Vec2(xf.fMin, yf.fMin), Vec2(xf.fMax - xf.fMin, yf.fMax - yf.fMin))
  }

  def isNormalised: Boolean = x.isNormalised && y.isNormalised

  def toFiniteRegionXY(sizeX: Double, sizeY: Double) = RegionXY(x.toFiniteInterval(sizeX), y.toFiniteInterval(sizeY))

}

object RegionXY {
  val all = RegionXY(Interval.all, Interval.all)
  def xEqualTo(x: Double) = RegionXY(Interval.equalTo(x), Interval.all)
  def yEqualTo(y: Double) = RegionXY(Interval.equalTo(y), Interval.all)
  def horizontalLine(y: Double) = yEqualTo(y)
  def verticalLine(x: Double) = xEqualTo(x)
  def apply(area: Option[Area]): RegionXY = area.map(_.toRegionXY).getOrElse(all)
}

