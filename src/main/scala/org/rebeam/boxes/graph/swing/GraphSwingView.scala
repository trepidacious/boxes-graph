package org.rebeam.boxes.graph.swing

import org.rebeam.boxes.swing._
import org.rebeam.boxes.swing.icons._
import org.rebeam.boxes.core._
import org.rebeam.boxes.core.util._
import org.rebeam.boxes.graph._

import java.awt.{BorderLayout, Graphics, Graphics2D, RenderingHints, AlphaComposite}
import java.awt.event.{ComponentAdapter, ComponentEvent, MouseEvent, MouseMotionListener, MouseListener}
import java.awt.geom.{Rectangle2D, AffineTransform}
import java.util.concurrent.{ExecutorService, Executors, Executor}
import javax.swing.SwingUtilities

import BoxTypes._
import BoxUtils._
import BoxScriptImports._

import GraphMouseEventType._
import GraphMouseButton._

import scalaz._
import Scalaz._

object GraphSwingView {

  val defaultExecutorPoolSize = 8
  val defaultThreadFactory = DaemonThreadFactory()
  lazy val defaultExecutor: Executor = Executors.newFixedThreadPool(defaultExecutorPoolSize, defaultThreadFactory)

  def icon(name: String) = IconFactory.icon(name)

  val zoom = icon("Zoom")
  val zoomIn = icon("ZoomIn")
  val zoomOut = icon("ZoomOut")
  val zoomSelect = icon("ZoomSelect")
  val boxSelect = icon("BoxSelect")
  val grabHand = icon("GrabHand")
  val move = icon("Move")

  def apply(graph: BoxScript[Graph]) = new GraphSwingView(graph)

}

class GraphSwingView(graph: BoxScript[Graph]) extends SwingView {

  val componentSize = atomic { create(Vec2(400, 400)) }
  val componentScaling = atomic { create(1.0) }

  val mainBuffer = new GraphBuffer()
  val overBuffer = new GraphBuffer()

  val component = new LinkingJPanel(this, new BorderLayout()) {

    def updateComponentScaling(s: Double) = for {
      currentScaling <- componentScaling()
      _ <- if (currentScaling != s) {
        componentScaling() = s
      } else {
        nothing
      }
    } yield ()

    override def paintComponent(gr: Graphics): Unit = {
      mainBuffer.display(gr)
      overBuffer.display(gr)

      //Update scaling for next redraw if necessary
      val s = SwingView.scalingOf(gr)
      atomic { updateComponentScaling(s) }
    }

    def updateSize(): Unit = {
      val v = Vec2(this.getWidth, this.getHeight)
      val s = SwingView.scalingOf(getGraphics())
      atomic { (componentSize() = v) andThen updateComponentScaling(s) }
    }

    this.addComponentListener(new ComponentAdapter() {
      override def componentResized(e: ComponentEvent): Unit = updateSize()
    })

    def fireMouse(e: MouseEvent, eventType: GraphMouseEventType): Unit =  {

      val p = e.getPoint
      val b = e.getButton match {
        case MouseEvent.BUTTON1 => LeftButton
        case MouseEvent.BUTTON2 => MiddleButton
        case MouseEvent.BUTTON3 => RightButton
        case _ => NoButton
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
  case class BufferDraw(buffer: GraphBuffer, spaces: GraphSpaces, highQuality: Boolean, scaling: Double, paints: List[(GraphCanvas) => Unit]) {
    def run(): Unit = {

      val w = spaces.componentArea.size.x.asInstanceOf[Int]
      val h = spaces.componentArea.size.y.asInstanceOf[Int]

      buffer.ensureRenderSize(spaces.componentArea.size * scaling)

      val g = buffer.imageToRender().getGraphics.asInstanceOf[Graphics2D]
      g.setTransform(AffineTransform.getScaleInstance(scaling, scaling))
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      val oldComposite = g.getComposite
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f))
      g.fill(new Rectangle2D.Double(0,0,w,h))
      g.setComposite(oldComposite)

      //Each layer paints on a fresh canvas, to avoid side effects from one affecting the next
      paints.foreach(_.apply(new GraphCanvasFromGraphics2D(g.create().asInstanceOf[Graphics2D], spaces, highQuality)))

      g.dispose

      //Swap buffer ready to be displayed
      buffer.swap(scaling)
    }
  }

  //Make a BufferDraw for main or overlayer according to boolean parameter
  def makeBufferDraw(useMainBuffer: Boolean): BoxScript[BufferDraw] = {
    val buffer = if (useMainBuffer) mainBuffer else overBuffer
    for {
      g <- graph
      highQuality <- g.highQuality
      spaces <- buildSpaces
      s <- componentScaling()
      layers <- if (useMainBuffer) g.layers else g.overlayers
      paints <- layers.traverseU(_.paint)
    } yield {
      BufferDraw(buffer, spaces, highQuality, s, paints)
    }
  }

  private def observer(useMainBuffer: Boolean) = Observer(

    //Script to make buffer draw, and effect to actually draw it to buffer. Then we
    //request a component repaint to display.
    makeBufferDraw(useMainBuffer),

    //Effect is to actually draw to the buffer in our default executor, then
    //invoke a repaint later in swing thread.
    (bd: BufferDraw) => {
      bd.run
      SwingView.later{ component.repaint() }
    },

    //Run script and effect in our default executor
    GraphSwingView.defaultExecutor, 

    //Only run on most recent relevant revision - no point redrawing for intermediate revisions
    true
  )

  //Observers for main and overlayer buffers. Note we
  //deliberately retain a reference to these so they won't be GCed. This view itself
  //is referenced from the LinkingJPanel we produce, and we reference the observers,
  //so observers will live as long as the JPanel is referenced.
  val mainObserver = observer(true)
  val overObserver = observer(false)

  //Register observers, note this doesn't result in any additional strong references to them
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

      // GraphSpacesLinear(
      //   dataArea = area, 
      GraphSpacesLog(
        desiredDataArea = area, 
        pixelArea = Area(Vec2(l, t+dh), Vec2(dw, -dh)),     //The pixel area to which we map data area when drawing data is within the graph borders  
        componentArea = Area(Vec2.zero, size)               //The component area is the whole of the JPanel component
      )
    }
  }

}
