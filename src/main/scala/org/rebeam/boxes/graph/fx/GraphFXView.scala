package org.rebeam.boxes.graph.fx

import org.rebeam.boxes.core._
import org.rebeam.boxes.core.util._
import org.rebeam.boxes.graph._
import org.rebeam.boxes.fx._
import org.rebeam.boxes.fx.Includes._

import java.util.concurrent.{ExecutorService, Executors, Executor}

import javafx.geometry.Insets
import javafx.scene.paint.Color
import javafx.scene.layout.StackPane
import javafx.scene.layout.Region
import javafx.scene.layout.Background
import javafx.scene.layout.BackgroundFill
import javafx.scene.layout.CornerRadii
import javafx.scene.input.MouseEvent
import javafx.scene.input.MouseButton
import javafx.event.EventHandler

import javafx.beans.property.ReadOnlyDoubleProperty


import BoxTypes._
import BoxUtils._
import BoxScriptImports._

import GraphMouseEventType._
import GraphMouseButton._

import scalaz._
import Scalaz._

object GraphFXView {

  val defaultWidth = 150d
  val defaultHeight = 100d

  val defaultExecutorPoolSize = 8
  val defaultThreadFactory = DaemonThreadFactory()
  lazy val defaultExecutor: Executor = Executors.newFixedThreadPool(defaultExecutorPoolSize, defaultThreadFactory)

  def apply(graph: BoxR[Graph]) = new GraphFXView(graph)
}

class LinkingStackPane(val linkTo: Any) extends StackPane()

class GraphFXView(graph: BoxR[Graph]) {

  val componentWidth = atomic { create(GraphFXView.defaultWidth) }
  val componentHeight = atomic { create(GraphFXView.defaultHeight) }

  def buildCanvas = for {
    w <- componentWidth
    h <- componentHeight
    componentSize = Vec2(w, h)
    s <- GraphUtils.buildSpaces(graph, componentSize)          
  } yield GraphCanvasFX(s)

  val component = new LinkingStackPane(this) {
    setPrefSize(GraphFXView.defaultWidth, GraphFXView.defaultHeight)
    setMaxSize(10000, 10000)
    setMinSize(10, 10)
  
    def fireMouse(e: MouseEvent, eventType: GraphMouseEventType): Unit =  {
    
      val b = e.getButton match {
        case MouseButton.PRIMARY => LeftButton
        case MouseButton.MIDDLE => MiddleButton
        case MouseButton.SECONDARY => RightButton
        case _ => NoButton
      }

      atomic {
        for {
          w <- componentWidth
          h <- componentHeight
          x = GraphUtils.clip(e.getX, 0, w)
          y = GraphUtils.clip(e.getY, 0, h)
          componentSize = Vec2(w, h)
          s <- GraphUtils.buildSpaces(graph, componentSize)          
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

    def eh(gmet: GraphMouseEventType): EventHandler[MouseEvent] = 
      new EventHandler[MouseEvent]() {
        def handle(e: MouseEvent): Unit = fireMouse(e, gmet)
      }
      
    setOnMouseDragged(eh(Drag))
    setOnMouseMoved(eh(Move))
    setOnMouseClicked(eh(Click))
    setOnMousePressed(eh(Press))
    setOnMouseReleased(eh(Release))
    setOnMouseEntered(eh(Enter))
    setOnMouseExited(eh(Exit))
        
  }
  
  component.widthProperty ==| componentWidth
  component.heightProperty ==| componentHeight
  
  component.getChildren().setAll(atomic { buildCanvas }.canvas, atomic { buildCanvas }.canvas)

  val bgf = new BackgroundFill(Color.LIGHTGREY, new CornerRadii(0), new javafx.geometry.Insets(0))
  component.setBackground(new Background(bgf))

  //All data needed to actually draw to a buffer, plus a method to perform the draw
  case class BufferDraw(spaces: GraphSpaces, paints: List[(GraphCanvas) => Unit])
  
  //Make a BufferDraw for main or overlayer according to boolean parameter
  def makeBufferDraw(useMainBuffer: Boolean): BoxScript[BufferDraw] = {
    for {
      w <- componentWidth
      h <- componentHeight
      componentSize = Vec2(w, h)
      spaces <- GraphUtils.buildSpaces(graph, componentSize)          
      g <- graph
      highQuality <- g.highQuality
      layers <- if (useMainBuffer) g.layers else g.overlayers
      paints <- layers.traverseU(_.paint)
    } yield {
      BufferDraw(spaces, paints)
    }
  }
  
  private def observer(useMainBuffer: Boolean) = Observer(
  
    //Get the spaces and paints we need to draw
    makeBufferDraw(useMainBuffer),
  
    //Now as an effect, draw to a new GraphCanvasFX, and then
    //schedule an action to set the new canvas as the appropriate child
    //of our main stackpane component
    (bd: BufferDraw) => {
      val gc = GraphCanvasFX(bd.spaces)
      bd.paints.foreach(paint => {
        paint.apply(gc)
        gc.resetStyle()
      })
      Fox.later{ component.getChildren().set(if (useMainBuffer) 0 else 1, gc.canvas) }
    },
  
    //Run script and effect in our default executor
    GraphFXView.defaultExecutor, 
  
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
  
}
