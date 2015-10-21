package org.rebeam.boxes.graph.swing

import org.rebeam.boxes.graph._
import org.rebeam.boxes.swing._

import java.awt.{Graphics, Graphics2D, Color, RenderingHints, BasicStroke, Image, AlphaComposite}
import java.awt.geom.{AffineTransform, Path2D, PathIterator, Rectangle2D}
import java.awt.image.{BufferedImage}

object GraphBuffer {
  val clearComposite = AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f)  
}

class GraphBuffer(var image: BufferedImage = new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB)) {

  val lock = new Object()
  
  def ensureSize(area:Vec2) {
    val w = math.max(area.x.asInstanceOf[Int], 1)
    val h = math.max(area.y.asInstanceOf[Int], 1)

    if (image.getWidth != w || image.getHeight != h) {
      image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    }
  }
  
  def clear() {
    val g = image.getGraphics.asInstanceOf[Graphics2D]
    g.setComposite(GraphBuffer.clearComposite)
    g.fill(new Rectangle2D.Double(0,0,image.getWidth,image.getHeight))
    g.dispose()
  }

  def draw(gr: Graphics): Unit = lock.synchronized{
    gr.drawImage(image, 0, 0, null)
  }
}
