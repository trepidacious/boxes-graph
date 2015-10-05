package org.rebeam.boxes.graph

import Axis._

//Immutable
case class Vec2(x: Double = 0, y: Double = 0) {
  def +(b: Vec2) = Vec2(x + b.x, y + b.y)
  def -(b: Vec2) = Vec2(x - b.x, y - b.y)
  def *(b: Vec2) = Vec2(x * b.x, y * b.y)
  def /(b: Vec2) = Vec2(x / b.x, y / b.y)
  def *(d: Double) = Vec2(d * x, d * y)
  def /(d: Double) = Vec2(x / d, y / d)
  def dot(b: Vec2) = x * b.x + y * b.y
  def transpose = Vec2(y, x)
  def withX(newX: Double) = Vec2(newX, y)
  def withY(newY: Double) = Vec2(x, newY)

  //The Vec2 that has the minimum value in each component that is in either
  //this Vec2, or b.
  //Geometrically, the bottom left corner of a rectangle
  //containing this Vec2 and b
  def componentMinima(b: Vec2) = Vec2(math.min(x, b.x), math.min(y, b.y))

  //The Vec2 that has the maximum value in each component that is in either
  //this Vec2, or b.
  //Geometrically, The top right corner of a rectangle
  //containing this Vec2 and b
  def componentMaxima(b: Vec2) = Vec2(math.max(x, b.x), math.max(y, b.y))
  def onAxis(axis: Axis) = axis match {
    case X => x
    case Y => y
  }
  def squaredLength = x * x + y * y
  def length = math.sqrt(squaredLength)
  
  //The union of the intervals defined by this Vec2 and b. Each interval is from
  //a minimum at the Vec2 x value to at maximum at the Vec2 y value.
  def intervalUnion(b: Vec2) = Vec2(math.min(x, b.x), math.max(y, b.y))
  
  def intervalContains(d: Double) = d >= math.min(x, y) && d <= math.max(x, y)
  
  def round() = Vec2(math.round(x), math.round(y))
}

object Vec2 {
  val zero = Vec2(0, 0)
  def x(v: Double = 1) = Vec2(v, 0)
  def y(v: Double = 1) = Vec2(0, v)
}