package org.rebeam.boxes.graph

import java.awt.Color
import java.awt.Image

trait GraphCanvas {
  def spaces: GraphSpaces
  def color_=(color: Color)
  def color: Color
  def lineWidth_=(w: Double)
  def lineWidth: Double
  def fontSize_=(w: Double)
  def fontSize: Double
  def dataLine(a: Vec2, b: Vec2)
  def line(a: Vec2, b: Vec2)
  def string(s: String, v: Vec2, align: Vec2 = Vec2.zero, rotateQuadrants:Int = 0)
  def stringSize(s: String): Vec2
  def rect(origin: Vec2, size: Vec2, fill: Boolean)
  def drawRect(origin: Vec2, size: Vec2)
  def fillRect(origin: Vec2, size: Vec2)
  def fillRect(area: Area)
  def drawRect(area: Area)
  def rect(area: Area, fill: Boolean)

  def fillRoundRect(origin: Vec2, size: Vec2, radius: Double)
  def fillRoundRect(area: Area, radius: Double)
  def drawRoundRect(origin: Vec2, size: Vec2, radius: Double)
  def drawRoundRect(area: Area, radius: Double)
  def roundRect(area: Area, fill: Boolean, radius: Double)
  def roundRect(origin: Vec2, size: Vec2, fill: Boolean, radius: Double)

  def clipToData()
  def clipToAll()
  def image(i: Image, origin: Vec2, size: Vec2)
  def image(i: Image, origin: Vec2)
  def image(i: Image, origin: Vec2, alpha: Double)
  def image(i: Image, origin: Vec2, size: Vec2, alpha: Double)
  def path(path: List[Vec2])
  def dataPath(path: List[Vec2])

  def drawTooltip(s: String, v: Vec2)
}