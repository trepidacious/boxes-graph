package org.rebeam.boxes.graph.swing

import org.rebeam.boxes.swing._
import org.rebeam.boxes.swing.icons._
import org.rebeam.boxes.core._
import org.rebeam.boxes.graph._

import java.awt.{BorderLayout, Graphics, Graphics2D, RenderingHints, AlphaComposite}
import java.awt.event.{ComponentAdapter, ComponentEvent, MouseEvent, MouseMotionListener, MouseListener}
import java.awt.geom.{Rectangle2D}

import BoxTypes._
import BoxUtils._
import BoxScriptImports._

import GraphMouseEventType._
import GraphMouseButton._

import scalaz._
import Scalaz._

object GraphSwingView {

  def icon(name: String) = IconFactory.icon(name)

  val zoom = icon("Zoom")
  val zoomIn = icon("ZoomIn")
  val zoomOut = icon("ZoomOut")
  val zoomSelect = icon("ZoomSelect")
  val boxSelect = icon("BoxSelect")
  val grabHand = icon("GrabHand")
  val move = icon("Move")

  def apply(graph: BoxScript[Graph]) = new GraphSwingView(graph)

//  //TODO can this be done with currying?
//  //Make a panel with series, using normal view
//  def panelWithSeries[K](
//    series: Box[List[Series[K]]],
//    selection: Box[Set[K]],
//
//    xName: Box[String],
//    yName: Box[String],
//    
//    xAxis: Box[GraphZoomerAxis],
//    yAxis: Box[GraphZoomerAxis],
//
//    graphName: Box[String],
//    
//    zoom: Boolean = true,
//    select: Boolean = false,
//    grab: Boolean = false,
//    
//    seriesTooltips: Boolean = false,
//    axisTooltips: Boolean = true,
//    
//    borders: Box[Borders],
//    extraMainLayers: List[GraphLayer] = List[GraphLayer](),
//    extraOverLayers: List[GraphLayer] = List[GraphLayer]()
//  )(implicit shelf: Shelf) = GraphSwing.panelWithSeries(g => GraphSwingView(g))(series, selection, xName, yName, xAxis, yAxis, graphName, zoom, select, grab, seriesTooltips, axisTooltips, borders, extraMainLayers, extraOverLayers)

}

//TODO background buffer drawing
class GraphSwingView(graph: BoxScript[Graph]) extends SwingView {

  val componentSize = atomic { create(Vec2(400, 400)) }

  val mainBuffer = new GraphBuffer()
  val overBuffer = new GraphBuffer()

  val component = new LinkingJPanel(this, new BorderLayout()) {

    override def paintComponent(gr: Graphics): Unit = {
      mainBuffer.draw(gr)
      overBuffer.draw(gr)
    }

    def updateSize(): Unit = {
      val v = Vec2(this.getWidth, this.getHeight)
      atomic { componentSize() = v }
    }

    this.addComponentListener(new ComponentAdapter() {
      override def componentResized(e: ComponentEvent): Unit = updateSize()
    })

    def fireMouse(e: MouseEvent, eventType: GraphMouseEventType): Unit =  {

      val p = e.getPoint
      val b = e.getButton match {
        case MouseEvent.BUTTON1 => Left
        case MouseEvent.BUTTON2 => Middle
        case MouseEvent.BUTTON3 => Right
        case _ => None
      }
      val w = getWidth
      val h = getHeight
      var x = GraphUtils.clip(p.x, 0, w)
      var y = GraphUtils.clip(p.y, 0, h)

      atomic {
        for {
          s <- buildSpaces          
          dataPoint = s.toData(Vec2(x, y))
          gme = GraphMouseEvent(s, dataPoint, eventType, b)
          consumedGME = GraphMouseEvent(s, dataPoint, Consumed, b)
          g <- graph
          ml <- g.layers
          ol <- g.overlayers
          l = ol ++ ml
          _ <- l.foldLeftM(false)((consumed, layer) => if (!consumed) layer.onMouse(gme) else layer.onMouse(consumedGME) andThen just(true))
        } yield ()
      }
    }

    this.addMouseMotionListener(new MouseMotionListener() {
      def mouseDragged(e: MouseEvent): Unit = fireMouse(e, Drag)
      def mouseMoved(e: MouseEvent): Unit = fireMouse(e, Move)
    })

    this.addMouseListener(new MouseListener(){
      def mouseClicked(e: MouseEvent): Unit = fireMouse(e, Click)
      def mousePressed(e: MouseEvent): Unit = fireMouse(e, Press)
      def mouseReleased(e: MouseEvent): Unit = fireMouse(e, Release)
      def mouseEntered(e: MouseEvent): Unit = fireMouse(e, Enter)
      def mouseExited(e: MouseEvent): Unit = fireMouse(e, Exit)
    })
  }

  //All data needed to actually draw to a buffer, plus a method to perform the draw
  case class BufferDraw(buffer: GraphBuffer, spaces: GraphSpaces, highQuality: Boolean, paints: List[(GraphCanvas) => Unit]) {
    def run(): Unit = buffer.lock.synchronized {

      val w = spaces.componentArea.size.x.asInstanceOf[Int]
      val h = spaces.componentArea.size.y.asInstanceOf[Int]

      buffer.ensureSize(spaces.componentArea.size)

      val g = buffer.image.getGraphics.asInstanceOf[Graphics2D]
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      val oldComposite = g.getComposite
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f))
      g.fill(new Rectangle2D.Double(0,0,w,h))
      g.setComposite(oldComposite)

      //Each layer paints on a fresh canvas, to avoid side effects from one affecting the next
      paints.foreach(_.apply(new GraphCanvasFromGraphics2D(g.create().asInstanceOf[Graphics2D], spaces, highQuality)))

      g.dispose
    }
  }

  //Make a BufferDraw for main or overlayer according to boolean parameter
  def makeBufferDraw(useMainBuffer: Boolean): BoxScript[BufferDraw] = {
    val buffer = if (useMainBuffer) mainBuffer else overBuffer
    for {
      g <- graph
      highQuality <- g.highQuality
      spaces <- buildSpaces
      layers <- if (useMainBuffer) g.layers else g.overlayers
      paints <- layers.traverseU(_.paint)
    } yield {
      BufferDraw(buffer, spaces, highQuality, paints)
    }
  }

  //TODO we can improve the threading here - there's no reason not to
  //run the scripts and drawing to buffer on a background thread, and only the
  //repaint request on the swing thread.
  //Observers for main and overlayer buffers
  val mainObserver = SwingView.observer(mainBuffer, makeBufferDraw(true)) { bd => 
    bd.run
    component.repaint()
  }

  val overObserver = SwingView.observer(overBuffer, makeBufferDraw(false)) { bd =>
    bd.run
    component.repaint()
  }

  atomic { observe(mainObserver) andThen observe(overObserver) }

  def buildSpaces: BoxScript[GraphSpaces] = {
    for {
      size <- componentSize() //The current size in pixels of the JPanel component we are using
      g <- graph
      area <- g.dataArea      
      borders <- g.borders
    } yield {
      val w = size.x.asInstanceOf[Int]
      val h = size.y.asInstanceOf[Int]

      val l = borders.left.asInstanceOf[Int]
      val r = borders.right.asInstanceOf[Int]
      val t = borders.top.asInstanceOf[Int]
      val b = borders.bottom.asInstanceOf[Int]
      val dw = w - l - r
      val dh = h - t - b

      GraphSpacesLinear(
        dataArea = area, 
        pixelArea = Area(Vec2(l, t+dh), Vec2(dw, -dh)),     //The pixel area to which we map data area when drawing data is within the graph borders  
        componentArea = Area(Vec2.zero, size)               //The component area is the whole of the JPanel component
      )
    }
  }

}
