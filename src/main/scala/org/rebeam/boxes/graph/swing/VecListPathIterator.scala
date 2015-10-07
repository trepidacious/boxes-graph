package org.rebeam.boxes.graph.swing

import org.rebeam.boxes.graph._
import org.rebeam.boxes.swing._

import java.awt.{Graphics2D, Color, RenderingHints, BasicStroke, Image, AlphaComposite}
import java.awt.geom.{AffineTransform, Path2D, PathIterator, Rectangle2D}
import java.awt.image.{BufferedImage}

object VecListPathIterator {
  def apply(path:List[Vec2]) = {
    val path2D = new Path2D.Double()
    path2D.append(new VecListPathIterator(path), false)
    path2D
  }
}

class VecListPathIterator(list:List[Vec2]) extends PathIterator {
  var remaining = list
  var first = true

  def getWindingRule = PathIterator.WIND_NON_ZERO
  def isDone = remaining.isEmpty
  def next() {
    remaining = remaining.tail
  }
  def currentSegment(coords:Array[Float]) = {
    coords.update(0, remaining.head.x.asInstanceOf[Float])
    coords.update(1, remaining.head.y.asInstanceOf[Float])
    if (first) {
      first = false
      PathIterator.SEG_MOVETO
    } else {
      PathIterator.SEG_LINETO
    }
  }
  def currentSegment(coords:Array[Double]) = {
    coords.update(0, remaining.head.x)
    coords.update(1, remaining.head.y)
    if (first) {
      first = false
      PathIterator.SEG_MOVETO
    } else {
      PathIterator.SEG_LINETO
    }
  }
}
