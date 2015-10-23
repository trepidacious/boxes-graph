package org.rebeam.boxes.graph.swing

import org.rebeam.boxes.graph._
import org.rebeam.boxes.swing._

import java.awt.{Graphics, Graphics2D, Color, RenderingHints, BasicStroke, Image, AlphaComposite}
import java.awt.geom.{AffineTransform, Path2D, PathIterator, Rectangle2D}
import java.awt.image.{BufferedImage}

object GraphBuffer {
  val clearComposite = AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f)  
}

class GraphBuffer() {

  private var renderImage: BufferedImage = new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB)
  private var displayImage: BufferedImage = new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB)

  val swapLock = new Object()
  
  def ensureRenderSize(area: Vec2): Unit = swapLock.synchronized {
    val w = math.max(area.x.asInstanceOf[Int], 1)
    val h = math.max(area.y.asInstanceOf[Int], 1)

    if (renderImage.getWidth != w || renderImage.getHeight != h) {
      renderImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    }
  }
  
  def clearRenderImage(): Unit = swapLock.synchronized {
    val g = renderImage.getGraphics.asInstanceOf[Graphics2D]
    g.setComposite(GraphBuffer.clearComposite)
    g.fill(new Rectangle2D.Double(0,0,renderImage.getWidth, renderImage.getHeight))
    g.dispose()
  }

  def display(gr: Graphics): Unit = swapLock.synchronized {
    gr.drawImage(displayImage, 0, 0, null)
  }

  def imageToRender(): BufferedImage = swapLock.synchronized { 
    renderImage 
  }

  def swap(): Unit = swapLock.synchronized {
    val i = displayImage
    displayImage = renderImage
    renderImage = i
  }
}
