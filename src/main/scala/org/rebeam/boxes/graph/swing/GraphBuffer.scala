package org.rebeam.boxes.graph.swing

import org.rebeam.boxes.graph._
import org.rebeam.boxes.swing._

import java.awt.{Graphics, Graphics2D, Color, RenderingHints, BasicStroke, Image, AlphaComposite, GraphicsEnvironment, Transparency}
import java.awt.geom.{AffineTransform, Path2D, PathIterator, Rectangle2D}
import java.awt.image.{BufferedImage}

object GraphBuffer {
  // val clearComposite = AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f)  
}

class GraphBuffer() {
  
  private var renderSize: Vec2 = Vec2(10, 10)
  private var displaySize: Vec2 = Vec2(10, 10)

  private var renderImage: Image = newImage(renderSize)
  private var displayImage: Image = newImage(displaySize)
  
  private var scaling = 1.0
  private var transform = AffineTransform.getScaleInstance(1.0, 1.0)

  val swapLock = new Object()
  
  private def newImage(size: Vec2) = {
    val w = math.max(size.x.asInstanceOf[Int], 1)
    val h = math.max(size.y.asInstanceOf[Int], 1)
    val env = GraphicsEnvironment.getLocalGraphicsEnvironment()
    val device = env.getDefaultScreenDevice()
    val config = device.getDefaultConfiguration()
    config.createCompatibleImage(w, h, Transparency.TRANSLUCENT)
    //Was: new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
  }
  
  def ensureRenderSize(area: Vec2): Unit = swapLock.synchronized {
    if (renderSize != area) {
      println("Making new image, was " + renderSize + ", need " + area + ".")
      renderImage = newImage(area)
      renderSize = area
    }
  }
  
  def display(gr: Graphics): Unit = {
    swapLock.synchronized {
      if (scaling == 1.0) {
        gr.asInstanceOf[Graphics2D].drawImage(displayImage, 0, 0, null)      
      } else {
        gr.asInstanceOf[Graphics2D].drawImage(displayImage, transform, null)
      }
      val t3 = System.nanoTime()
    }
  }

  def imageToRender(): Image = swapLock.synchronized { 
    renderImage 
  }

  def swap(newScaling: Double): Unit = swapLock.synchronized {
    if (scaling != newScaling) {
      scaling = newScaling
      val s = 1.0/scaling
      transform = AffineTransform.getScaleInstance(s, s)
    }    
    val i = displayImage
    val s = displaySize
    displayImage = renderImage
    displaySize = renderSize
    renderImage = i
    
    renderSize = s
  }
}
