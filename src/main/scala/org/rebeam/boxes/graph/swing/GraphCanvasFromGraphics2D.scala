package org.rebeam.boxes.graph.swing

import org.rebeam.boxes.graph._
import org.rebeam.boxes.swing._

import java.awt.{Graphics2D, Color, RenderingHints, BasicStroke, Image, AlphaComposite}
import java.awt.geom.{AffineTransform, Path2D, PathIterator, Rectangle2D, Line2D}
import java.awt.image.{BufferedImage}

class GraphCanvasFromGraphics2D(g: Graphics2D, val spaces: GraphSpaces, val highQuality: Boolean, scaling: Double) extends GraphCanvas {

  val highDPIScaling = 1.1

  val defaultClip = g.getClip
  val defaultFont = g.getFont

  var c = Color.black
  var w = 1d
  var fs = 10d

  //Start with default line quality
  defaultLineQuality()

  val highDPI = scaling > highDPIScaling

  def color_=(color: Color) {
    g.setColor(color)
    c = color
  }
  def color = c

  def lineWidth_=(lineWidth: Double) {
    g.setStroke(new BasicStroke(lineWidth.asInstanceOf[Float], BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER))
    w = lineWidth
  }
  def lineWidth = w

  def fontSize_=(fontSize: Double) {
    g.setFont(defaultFont.deriveFont(fontSize.asInstanceOf[Float]))
    fs = fontSize
  }
  def fontSize = fs

  private def dataLineQuality() {
    if (highQuality) {
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
      g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_DEFAULT)
    } else {
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_DEFAULT)
      g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED)
    }    
  }
  
  private def defaultLineQuality() {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    //On high DPI screens, use pure stroke rendering for guaranteed subpixel alignment - we shouldn't be able to see any fuzzy anti-aliasing
    if (highDPI) {
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
    } else {
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_DEFAULT)
    }
    g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_DEFAULT)
  }
  
  def dataLine(a: Vec2, b: Vec2) {
    dataLineQuality()
    //Allow use of subpixel line rendering only in high quality mode
    line(spaces.toPixel(a), spaces.toPixel(b), highQuality)
    defaultLineQuality()
  }

  def line(a: Vec2, b: Vec2) {
    line(a, b, true)
  }

  private val lineShape = new Line2D.Double(0, 0, 0, 0)

  private def line(a: Vec2, b: Vec2, subPixel: Boolean) {
    //If we are on a high DPI screen, draw to subpixel accuracy if requested
    if (highQuality && (highDPI)) {
      lineShape.x1 = a.x
      lineShape.y1 = a.y
      lineShape.x2 = b.x
      lineShape.y2 = b.y
      g.draw(lineShape)
    } else {
      g.drawLine(a.x.asInstanceOf[Int], a.y.asInstanceOf[Int], b.x.asInstanceOf[Int], b.y.asInstanceOf[Int])
    }
  }

  def stringSize(s: String) = {
    val d = g.getFontMetrics.getStringBounds(s, g)
    Vec2(d.getWidth, d.getHeight)
  }

  def string(s: String, v: Vec2, align: Vec2 = Vec2.zero, rotateQuadrants: Int = 0) {

    val d = g.getFontMetrics.getStringBounds(s, g)
    val w = d.getWidth
    val h = d.getHeight

    val x = v.x.asInstanceOf[Int]
    val y = v.y.asInstanceOf[Int]

    val oldxForm = g.getTransform
    val t = g.getTransform
    t.concatenate(AffineTransform.getQuadrantRotateInstance(rotateQuadrants, x, y))
    g.setTransform(t)

    val vo = v + Vec2(-w * align.x, h * align.y)

    if (highDPI) {
      val ox = vo.x.asInstanceOf[Float]
      val oy = vo.y.asInstanceOf[Float]
      g.drawString(s, ox, oy)
    } else {
      val ox = vo.x.asInstanceOf[Int]
      val oy = vo.y.asInstanceOf[Int]
      g.drawString(s, ox, oy)
    }

    g.setTransform(oldxForm)
  }

  //Convert from a pair of vecs that may draw a "backwards"
  //rect with negative size(s) to the dumb format needed by Java2D,
  //with individual int values that must have a positive width and height
  private def toDumbFormat(origin: Vec2, size: Vec2) = {
    var x = origin.x.asInstanceOf[Int]
    var y = origin.y.asInstanceOf[Int]
    var w = size.x.asInstanceOf[Int]
    var h = size.y.asInstanceOf[Int]
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

  private val rectShape = new Rectangle2D.Double(0.0, 0.0, 1.0, 1.0)

  private def toRectShape(origin: Vec2, size: Vec2) = {
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
    rectShape.x = x
    rectShape.y = y
    rectShape.width = w
    rectShape.height = h
  }

  def rect(origin: Vec2, size: Vec2, fill: Boolean) {
    if (highDPI) {
      toRectShape(origin, size)
      if (fill) {
        g.fill(rectShape)
      } else {
        g.draw(rectShape)
      }
    } else {
      val df = toDumbFormat(origin, size)
      if (fill) {
        g.fillRect(df._1, df._2, df._3, df._4)
      } else {
        g.drawRect(df._1, df._2, df._3, df._4)
      }      
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
    val r = radius.asInstanceOf[Int]
    if (fill) {
      g.fillRoundRect(df._1, df._2, df._3, df._4, r, r)
    } else {
      g.drawRoundRect(df._1, df._2, df._3, df._4, r, r)
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
    val df = toDumbFormat(origin, size)
    g.setClip(df._1, df._2, df._3, df._4)
  }

  def clipToData() {
    clipToRect(spaces.pixelArea.origin, spaces.pixelArea.size)
  }

  def clipToAll() {
    g.setClip(defaultClip)
  }

  def image(i: Image, origin: Vec2, size: Vec2, alpha: Double) {
    // TODO should we use dumb format, or leave potentially mirrored image? Check whether mirrored image actually draws.
//    val df = toDumbFormat(origin, size)
    val oc = g.getComposite()
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha.floatValue))
    g.drawImage(i, origin.x.asInstanceOf[Int], origin.y.asInstanceOf[Int], size.x.asInstanceOf[Int], size.y.asInstanceOf[Int], null)
    g.setComposite(oc)
  }

  def image(i: Image, origin: Vec2, alpha: Double) {
    image(i, origin, Vec2(i.getWidth(null), i.getHeight(null)), alpha)
  }

  def image(i: Image, origin: Vec2, size: Vec2) {
    // TODO should we use dumb format, or leave potentially mirrored image? Check whether mirrored image actually draws.
//    val df = toDumbFormat(origin, size)
    g.drawImage(i, origin.x.asInstanceOf[Int], origin.y.asInstanceOf[Int], size.x.asInstanceOf[Int], size.y.asInstanceOf[Int], null)
  }

  def image(i: Image, origin: Vec2) {
    image(i, origin, Vec2(i.getWidth(null), i.getHeight(null)))
  }

  def path(path: List[Vec2]) {
    val path2D = new Path2D.Double()
    path2D.append(new VecListPathIterator(path), false)
    g.draw(path2D)
  }

  def dataPath(dataPath: List[Vec2]) {
    dataLineQuality()
    path(dataPath.map(p => spaces.toPixel(p)))
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
