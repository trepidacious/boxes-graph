package org.rebeam.boxes.graph

import Axis._

trait Interval {
  def min: Option[Double]
  def max: Option[Double]

  def contains(d: Double): Boolean = (min, max) match {
    case (None, None) => true
    case (Some(min), None) => d >= min
    case (None, Some(max)) => d <= max
    case (Some(min), Some(max)) => (d >= Math.min(min, max)) && (d <= Math.max(min, max))
  }

  def normalised = (min, max) match {
    case (Some(min), Some(max)) if min > max => FiniteInterval(max, min)    
    case _ => this
  }

  /**
   * This will combine two intervals to yield an "outer" interval.
   * Note that this is NOT an interval that contains all points in
   * the two intervals (union) nor an interval that contains only
   * points in both intervals (intersection). Instead it only considers
   * defined limits (Some(value)), and takes the outer defined limits
   * for both min and max. So this gives an interval whose defined limits
   * are contain the defined limits of the two intervals, and which only
   * has undefined limits where both intervals have undefined limits.
   * A use case for this is to produce a sensible "view interval" for
   * showing data in either of the two intervals. Since we can't hope to
   * display out to infinity, we just make sure that we display at least the
   * defined (finite) limits of both intervals. 
   */
  def definedOuter(i: Interval) = {
    val l = List(this.normalised, i.normalised)

    val min = l.map(_.min).flatten.sorted.headOption
    val max = l.map(_.max).flatten.sorted.reverse.headOption

    Interval(min, max)
  }

  def isFinite = min.isDefined && max.isDefined

  def isNormalised: Boolean = (min, max) match {
    case (Some(min), Some(max)) if min > max => false
    case _ => true
  }

  def toFiniteInterval(size: Double): FiniteInterval = {
    val s = Math.abs(size)
    (min, max) match {
      case (Some(min), Some(max)) => if (min <= max) FiniteInterval(min, max) else FiniteInterval(max, min)
      case (Some(min), None) => FiniteInterval(min, min + s)
      case (None, Some(max)) => FiniteInterval(max - s, max)
      case (None, None) => FiniteInterval(0, s)
    }
  }
}

case class FiniteInterval(fMin: Double, fMax: Double) extends Interval {
  val min = Some(fMin)
  val max = Some(fMax)
}

object Interval {
  
  def apply(min: Option[Double], max: Option[Double]): Interval = new IntervalOption(min, max)
  def apply(min: Double, max: Double): FiniteInterval = new FiniteInterval(min, max)

  val all = Interval(None, None)

  def greaterThanOrEqualTo(v: Double) = Interval(Some(v), None)

  def notLessThan(v: Double) = Interval(Some(v), None)

  def lessThanOrEqualTo(v: Double) = Interval(None, Some(v))

  def notGreaterThan(v: Double) = lessThanOrEqualTo(v)

  def equalTo(v: Double) = FiniteInterval(v, v)

}

/**
 * Represents a (possibly unbounded) interval of Doubles
 * If min is specified and max is unspecified, represents interval of values x where x >= min
 * If max is specified and min is unspecified, represents interval of values x where x <= max
 * If min and max are specified, represents values x where a <= x <= b, where a is the lesser of min and max, and b is the greater
 * If neither min nor max are specified, represents interval containing all values
 * Can be normalised to ensure min <= max, but note that denormalised 
 * intervals are still treated as valid by swapping min and max as described above.
 */
case class IntervalOption(min: Option[Double], max: Option[Double]) extends Interval
