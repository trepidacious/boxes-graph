package org.rebeam.boxes.graph.fx

import org.rebeam.boxes.graph._
import org.rebeam.boxes.swing._

import javafx.scene.canvas.Canvas
import javafx.scene.canvas.GraphicsContext
import javafx.scene.paint.Color
import javafx.scene.text.Font
import javafx.scene.text.Text;
import javafx.scene.transform.Affine

import scala.language.implicitConversions

object GraphCanvasFX {
  def apply(spaces: GraphSpaces) = new GraphCanvasFX(spaces)
}

class GraphCanvasFX(val spaces: GraphSpaces) extends GraphCanvas {

  val canvas = new Canvas(spaces.componentArea.size.x, spaces.componentArea.size.y)
	val g = canvas.getGraphicsContext2D()

  implicit def awt2JavaFXColor(c: java.awt.Color) = Color.rgb(c.getRed(), c.getGreen(), c.getBlue(), c.getAlpha() / 255.0)
  
  var c = java.awt.Color.black
  var w = 1d
  var fs = 10d
  
  //Track whether we have clipped or not - if we have, we will have a saved
  //state we can restore to undo the clipping
  var clipped = false

  defaultLineQuality()

  def resetStyle(): Unit = {
    defaultLineQuality()
    color = java.awt.Color.black
    lineWidth = 1d
    fontSize = 10d
    clipToAll()
  }


  def color_=(color: java.awt.Color) {
    g.setFill(color)
    g.setStroke(color)
    c = color
  }
  def color = c

  def lineWidth_=(lineWidth: Double) {
    g.setLineWidth(lineWidth)
    w = lineWidth
  }
  def lineWidth = w

  def fontSize_=(fontSize: Double) {
    g.setFont(Font.font(fontSize))
    fs = fontSize
  }
  def fontSize = fs

  private def dataLineQuality() {}

  private def defaultLineQuality() {}
  
  def dataLine(a: Vec2, b: Vec2) {
    dataLineQuality()
    line(spaces.toPixel(a), spaces.toPixel(b))
    defaultLineQuality()
  }

  def line(a: Vec2, b: Vec2)  = g.strokeLine(a.x, a.y, b.x, b.y)

  private val text = new Text("");

  def stringSize(s: String) = {
    text.setFont(g.getFont())
    text.setText(s)
    Vec2(text.getLayoutBounds().getWidth(), text.getLayoutBounds().getHeight())
  }

  def string(s: String, v: Vec2, align: Vec2 = Vec2.zero, rotateQuadrants: Int = 0) {
    val d = stringSize(s)
    val w = d.x
    val h = d.y

    val oldxForm = g.getTransform
    val t = g.getTransform
    t.appendRotation(rotateQuadrants * 90, v.x, v.y)
    g.setTransform(t)

    val vo = v + Vec2(-w * align.x, h * align.y)

    g.fillText(s, vo.x, vo.y)

    g.setTransform(oldxForm)
  }

  //Convert from a pair of vecs that may draw a "backwards"
  //rect with negative size(s) to the dumb format needed by JavaFX,
  //with individual double values that must have a positive width and height
  private def toDumbFormat(origin: Vec2, size: Vec2) = {
    var x = origin.x
    var y = origin.y
    var w = size.x
    var h = size.y
    if (h < 0) {
      y = y + h
      h = -h
    }
    if (w < 0) {
      x = x + w
      w = -w
    }
    (x, y, w, h)
  }

  def rect(origin: Vec2, size: Vec2, fill: Boolean) {
    val df = toDumbFormat(origin, size)
    if (fill) {
      g.fillRect(df._1, df._2, df._3, df._4)
    } else {
      g.strokeRect(df._1, df._2, df._3, df._4)
    }      
  }

  def fillRect(area: Area) {
    rect(area, true)
  }
  def drawRect(area: Area) {
    rect(area, false)
  }
  def rect(area: Area, fill: Boolean) {
    rect(area.origin, area.size, fill)
  }

  def fillRect(origin: Vec2, size: Vec2) {
    rect(origin, size, true)
  }

  def drawRect(origin: Vec2, size: Vec2) {
    rect(origin, size, false)
  }

  def roundRect(origin: Vec2, size: Vec2, fill: Boolean, radius: Double) {
    val df = toDumbFormat(origin, size)
    if (fill) {
      g.fillRoundRect(df._1, df._2, df._3, df._4, radius, radius)
    } else {
      g.strokeRoundRect(df._1, df._2, df._3, df._4, radius, radius)
    }
  }

  def roundRect(area: Area, fill: Boolean, radius: Double) {
    roundRect(area.origin, area.size, fill, radius)
  }

  def fillRoundRect(origin: Vec2, size: Vec2, radius: Double) {
    roundRect(origin, size, true, radius)
  }
  def fillRoundRect(area: Area, radius: Double) {
    roundRect(area, true, radius)
  }
  def drawRoundRect(origin: Vec2, size: Vec2, radius: Double){
    roundRect(origin, size, false, radius)
  }
  def drawRoundRect(area: Area, radius: Double){
    roundRect(area, false, radius)
  }

  def clipToRect(origin: Vec2, size: Vec2) {
    //Remove any existing clip
    clipToAll()
    //Save so we can (somewhat messily) get rid of the clip again
    g.save()
    //Apply the actual clip
    //TODO do we REALLY need to draw out a rectangle here?
    g.beginPath()
    g.moveTo(origin.x, origin.y)
    g.lineTo(origin.x + size.x, origin.y)
    g.lineTo(origin.x + size.x, origin.y + size.y)
    g.lineTo(origin.x, origin.y + size.y)
    g.closePath()
    g.clip()
    //Remember we are clipped so we will restore on clipToAll
    clipped = true
  }

  def clipToData() {
    clipToRect(spaces.pixelArea.origin, spaces.pixelArea.size)
  }

  def clipToAll() = if (clipped) {
    //Restore to get our saved (no) clip state back,
    //this also restores everything else, so reapply
    //our settings
    g.restore()
    g.setFill(color)
    g.setStroke(color)
    g.setLineWidth(lineWidth)
    g.setFont(Font.font(fontSize))    
  }
  
  def image(i: java.awt.Image, origin: Vec2, size: Vec2, alpha: Double) {
    //TODO
  }

  def image(i: java.awt.Image, origin: Vec2, alpha: Double) {
    image(i, origin, Vec2(i.getWidth(null), i.getHeight(null)), alpha)
  }

  def image(i: java.awt.Image, origin: Vec2, size: Vec2) {
    //TODO
  }

  def image(i: java.awt.Image, origin: Vec2) {
    image(i, origin, Vec2(i.getWidth(null), i.getHeight(null)))
  }

  def path(path: List[Vec2]) {
    g.beginPath()
    val h = path.head
    g.moveTo(h.x, h.y)
    for (v <- path.tail) {
      g.lineTo(v.x, v.y)
    }
    g.stroke()
  }

  def dataPath(dataPath: List[Vec2]) {
    dataLineQuality()
    path(dataPath map spaces.toPixel)
    defaultLineQuality()
  }
  
  def drawTooltip(s: String, v: Vec2) {
    val size = stringSize(s)

    val offRight = (v.round + size).x - spaces.pixelArea.axisBounds(Axis.X)._2 + 8;
    val pp = if (offRight > 0) {
      v.round - (Vec2(offRight, 0))
    } else {
      v.round
    }
    
    color = SwingView.shadedBoxColor
    fillRoundRect(pp - Vec2(-8, 4 + size.y + 8), size + Vec2(8, 8), 6)

    color = SwingView.selectedTextColor
    string(s, pp + Vec2(12, -12))
  }
  
}
