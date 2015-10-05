package org.rebeam.boxes.graph

import Axis._

case class Area(origin:Vec2 = Vec2(), size:Vec2 = Vec2(1, 1)) {
  def toUnit(v:Vec2) = (v - origin)/size
  def fromUnit(v:Vec2) = (v * size) + origin
  def axisBounds(a:Axis) = a match {
    case X => (origin.x, origin.x + size.x)
    case Y => (origin.y, origin.y + size.y)
  }
  def axisContains(a:Axis, p: Double) = a match {
    case X => (p >= origin.x && p<= origin.x + size.x)
    case Y => (p >= origin.y && p<= origin.y + size.y)
  }
  def axisSize(a:Axis) = a match {
    case X => size.x
    case Y => size.y
  }
  def axisRelativePosition(a:Axis, v:Vec2) = a match {
    case X => (v - origin).x
    case Y => (v - origin).y
  }
  def axisToUnit(a:Axis, v:Vec2) = a match {
    case X => toUnit(v).x
    case Y => toUnit(v).y
  }
  def axisPosition(a:Axis, p:Double) = a match {
    case X => Vec2(p, origin.y)
    case Y => Vec2(origin.x, p)
  }
  def axisVec2(a:Axis) = a match {
    case X => Vec2(size.x, 0)
    case Y => Vec2(0, size.y)
  }
  def axisPerpVec2(a:Axis) = a match {
    case X => Vec2(0, size.y)
    case Y => Vec2(size.x, 0)
  }
  def round() = {
    val o = origin.round()
    val c = (origin + size).round()
    Area(o, c-o)
  }
  def contains(v:Vec2) = normalise.rawContains(v)
  private def rawContains(v:Vec2) = (v.x >= origin.x && v.y >= origin.y && v.x <= origin.x + size.x && v.y <= origin.y + size.y)

  def contains(a:Area) = normalise.rawContains(a)
  private def rawContains(a:Area) = {
    val area = a.normalise
    (area.origin.x >= origin.x && area.origin.y >= origin.y && area.origin.x + area.size.x <= origin.x + size.x && area.origin.y + area.size.y <= origin.y + size.y)
  }

  def normalise = {
    var w = size.x
    var h = size.y
    if (w >= 0 && h >= 0 ) {
      this
    } else {
      var x = origin.x
      var y = origin.y
      if (h < 0) {
        y = y + h
        h = -h
      }
      if (w < 0) {
        x = x + w
        w = -w
      }
      Area(Vec2(x, y), Vec2(w, h))
    }
  }
  def translate(v: Vec2) = Area(origin + v, size)
  def extendToContain(v:Vec2) = {
    if (contains(v)) {
      this
    } else {
      normalise.rawExtendToContain(v)
    }
  }
  private def rawExtendToContain(v:Vec2) = {
    //TODO can probably be done more concisely using corners
    var x = origin.x
    var y = origin.y
    var w = size.x
    var h = size.y
    if (v.x < x) {
      w += x-v.x
      x = v.x
    } else if (v.x > x + w) {
      w = v.x - x
    }
    if (v.y < y) {
      h += y-v.y
      y = v.y
    } else if (v.y > y + h) {
      h = v.y - y
    }
    Area(Vec2(x, y), Vec2(w, h))
  }

  def extendToContain(a:Area) = {
    if (contains(a)) {
      this
    } else {
      normalise.rawExtendToContain(a.normalise)
    }
  }
  private def rawExtendToContain(a:Area) = {

    //TODO can probably be done more concisely using corners
    var x = origin.x
    var y = origin.y
    var w = size.x
    var h = size.y
    if (a.origin.x < x) {
      w += x - a.origin.x
      x = a.origin.x
    }
    if (a.origin.x + a.size.x > x + w) {
      w = a.origin.x + a.size.x - x
    }
    if (a.origin.y < y) {
      h += y - a.origin.y
      y = a.origin.y
    }
    if (a.origin.y + a.size.y > y + h) {
      h = a.origin.y + a.size.y - y
    }
    Area(Vec2(x, y), Vec2(w, h))
  }

  def intersection(a:Area) = normalise.rawIntersection(a.normalise)

  def rawIntersection(a:Area) = {
    //Find corners of the intersection
    val bottomLeft = origin.componentMaxima(a.origin)
    val topRight = (origin + size).componentMinima(a.origin + a.size)

    //Produce area from corners
    Area(bottomLeft, topRight - bottomLeft)
  }
  
  private def left = origin.x
  private def right = origin.x + size.x
  private def bottom = origin.y
  private def top = origin.y + size.y
  
  def intersects(a:Area) = normalise.rawIntersects(a.normalise)
  def rawIntersects(a:Area) = !(
    a.left > right || 
    a.right < left || 
    a.top < bottom ||
    a.bottom > top
  )

  def pad(v:Vec2): Area = pad(v, v)
  def pad(before: Vec2, after: Vec2): Area = normalise.rawPad(before, after)
  def rawPad(before: Vec2, after: Vec2) = {
    val beforePadding = size * before
    val o = origin - beforePadding
    val s = size + beforePadding + size * after
    Area(o, s)
  }

  def inset(r: Double, u: Double, l: Double, d: Double): Area = normalise.rawInset(r, u, l, d)
  def rawInset(r: Double, u: Double, l: Double, d: Double) = {
    val o = origin + Vec2(l, d)
    val s = size + Vec2(-l-r, -u-d)
    Area(o, s)
  }

  def sizeAtLeast(minSize:Vec2) = normalise.rawSizeAtLeast(minSize)
  def rawSizeAtLeast(minSize:Vec2) = {
    if (size.x >= minSize.x && size.y >= minSize.y) {
      this
    } else {
      val newSize = Vec2(math.max(size.x, minSize.x), math.max(size.y, minSize.y))
      val newOrigin = origin - (newSize - size) / 2
      Area(newOrigin, newSize)
    }
  }
  
  def replaceAxis(axis: Option[Axis], area: Area) = axis match {
    case Some(Axis.X) => Area(Vec2(area.origin.x, origin.y), Vec2(area.size.x, size.y))
    case Some(Axis.Y) => Area(Vec2(origin.x, area.origin.y), Vec2(size.x, area.size.y))
    case _ => this
  }
}