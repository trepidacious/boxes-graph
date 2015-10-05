package org.rebeam.boxes.graph

import org.rebeam.boxes.core._
import org.rebeam.boxes.swing.SwingView
import org.rebeam.boxes.swing.icons.IconFactory
import BoxScriptImports._
import BoxTypes._

import java.awt.Color
import java.awt.geom.Rectangle2D
import java.text.DecimalFormat

import GraphMouseEventType._
import Axis._

trait GraphLayer {
  //When called, reads Box state and returns a method that will draw this state to a canvas
  def paint: BoxScript[(GraphCanvas) => Unit]

  //Handle an event, returns false to allow it to reach other layers, or true to consume it
  def onMouse(event: GraphMouseEvent): BoxScript[Boolean]
  
  def dataBounds: BoxScript[Option[Area]]
}

trait GraphDisplayLayer extends GraphLayer {
  def onMouse(event: GraphMouseEvent) = just(false)
}

abstract class UnboundedGraphDisplayLayer extends GraphDisplayLayer {
  val dataBounds = just(None:Option[Area])
}

abstract class UnboundedGraphLayer extends GraphLayer {
  val dataBounds = just(None:Option[Area])
}

abstract class UnboundedInputGraphLayer extends UnboundedGraphLayer {
  val paint = just((_:GraphCanvas) => ())
}

class GraphSeries[K](series: BoxScript[List[Series[K]]], shadow: BoxScript[Boolean] = just(true)) extends GraphDisplayLayer {

  def paint = for {
    currentSeries <- series
    currentShadow <- shadow
  } yield {
    (canvas:GraphCanvas) => {
      canvas.clipToData
      for (s <- currentSeries) {
        s.painter.paint(canvas, s, currentShadow)
      }
    }
  }

  val dataBounds = for {
    currentSeries <- series
  } yield {
    currentSeries.foldLeft(None: Option[Area]){(seriesArea, series) => series.curve.foldLeft(seriesArea){
      (area, v) => area match {
        case None => Some(Area(v, Vec2.zero))
        case Some(a) => Some(a.extendToContain(v))
      }
    }}
  }
}

trait Graph {
  def layers: BoxScript[List[GraphLayer]]
  def overlayers: BoxScript[List[GraphLayer]]
  def dataArea: BoxScript[Area]
  def borders: BoxScript[Borders]
  def highQuality: BoxScript[Boolean]
}

case class GraphZoomerAxis(
  requiredRange: BoxScript[Option[(Double, Double)]],
  paddingBefore: BoxScript[Double],
  paddingAfter: BoxScript[Double],
  minSize: BoxScript[Double]
)

object GraphDefaults {
  def axis = GraphZoomerAxis(just(None), just(0.05), just(0.05), just(0.01))
  def axis(paddingBefore: Double, paddingAfter: Double) = GraphZoomerAxis(just(None), just(paddingBefore), just(paddingAfter), just(0.01))
}

class GraphBG(val bg: Color, val dataBG: Color) extends UnboundedGraphDisplayLayer {
  def paint = just (
    (canvas: GraphCanvas) => {
      canvas.color = bg
      canvas.fillRect(canvas.spaces.componentArea.origin, canvas.spaces.componentArea.size)

      canvas.color = dataBG
      canvas.fillRect(canvas.spaces.pixelArea.origin, canvas.spaces.pixelArea.size)
    }
  )
}

class GraphOutline extends UnboundedGraphDisplayLayer {
  def paint = just (
    (canvas:GraphCanvas) => {
      canvas.color = GraphAxis.axisColor
      canvas.drawRect(canvas.spaces.pixelArea.origin, canvas.spaces.pixelArea.size)
    }
  )
}

class GraphHighlight extends UnboundedGraphDisplayLayer {
  def paint = just (
    (canvas:GraphCanvas) => {
      canvas.color = SwingView.alternateBackgroundColor.brighter
      canvas.drawRect(canvas.spaces.pixelArea.origin + Vec2(-1, 1), canvas.spaces.pixelArea.size + Vec2(1, -1))
    }
  )
}

object GraphBusy {
  val pencil = IconFactory.image("GraphPencil")
}

class GraphBusy(val alpha: BoxScript[Double]) {//extends UnboundedGraphDisplayLayer {
  def paint = for {
    a <- alpha
  } yield (canvas:GraphCanvas) => if (a > 0) {
    val pa = canvas.spaces.pixelArea
    canvas.image(GraphBusy.pencil, pa.origin + pa.size + Vec2(-43, 10), a * 0.5)
  }
}

object GraphShadow {
  val topLeft = IconFactory.image("GraphShadowTopLeft")
  val top = IconFactory.image("GraphShadowTop")
  val left = IconFactory.image("GraphShadowLeft")
}

class GraphShadow extends UnboundedGraphDisplayLayer {
  def paint = just (
    (canvas:GraphCanvas) => {
      canvas.clipToData
      val w = GraphShadow.topLeft.getWidth(null)
      val h = GraphShadow.topLeft.getHeight(null)

      val a = canvas.spaces.pixelArea
      val tl = a.origin + a.size.withX(0)
      val bl = a.origin
      val br = a.origin + a.size.withY(0)

      canvas.image(GraphShadow.top, tl, Vec2(a.size.x, h))
      canvas.image(GraphShadow.left, tl, Vec2(w, -a.size.y))
      canvas.image(GraphShadow.top, bl + Vec2(0, 2), Vec2(a.size.x, -h))
      canvas.image(GraphShadow.left, br + Vec2(2, 0), Vec2(-w, a.size.y))
    }
  )
}

object GraphAxis {
  val fontSize = 10
  val titleFontSize = 12
  val fontColor = SwingView.textColor
  val axisColor = SwingView.dividingColor
  val axisHighlightColor = SwingView.alternateBackgroundColor.brighter
  val gridMajorColor = new Color(0f, 0f, 0f, 0.08f)
  val gridMinorColor = new Color(0f, 0f, 0f, 0.03f)
  val defaultFormat = new DecimalFormat("0.###")

  def apply(axis: Axis, pixelsPerMajor: Int = 100, format: DecimalFormat = GraphAxis.defaultFormat) = new GraphAxis(axis, pixelsPerMajor, format)
}

class GraphAxis(val axis: Axis, val pixelsPerMajor: Int = 100, val format: DecimalFormat = GraphAxis.defaultFormat, val gridlines: Boolean = true) extends UnboundedGraphDisplayLayer {

  def paint = just (
    (canvas:GraphCanvas) => {
      val dataArea = canvas.spaces.dataArea

      val ticks = Ticks(dataArea.axisBounds(axis), canvas.spaces.pixelArea.axisSize(axis), pixelsPerMajor)

      ticks.foreach(t => {
        val (p, major) = t
        val start = canvas.spaces.toPixel(dataArea.axisPosition(axis, p))

        canvas.color = GraphAxis.axisColor
        axis match {
          case X => canvas.line(start, start + Vec2(0, if (major) 8 else 4))
          case Y => canvas.line(start, start + Vec2(if (major) -8 else -4, 0))
        }
        canvas.color = GraphAxis.axisHighlightColor
        axis match {
          case X => canvas.line(start + Vec2(1, 0), start + Vec2(1, if (major) 8 else 4))
          case Y => canvas.line(start + Vec2(0, 1), start + Vec2(if (major) -8 else -4, 1))
        }


        if (major) {
          canvas.color = GraphAxis.fontColor
          canvas.fontSize = GraphAxis.fontSize
          axis match {
            case X => canvas.string(format.format(p), start + Vec2(0, 10), Vec2(0.5, 1))
            case Y => canvas.string(format.format(p), start + Vec2(-10, 0), Vec2(1, 0.5))
          }
          if (gridlines) {
            canvas.color = GraphAxis.gridMajorColor
            canvas.line(start, start + canvas.spaces.pixelArea.axisPerpVec2(axis))
          }
        }
//FIXME get rid of this?
//        } else {
//          canvas.color = GraphAxis.gridMinorColor
//          canvas.line(start, start + canvas.spaces.pixelArea.axisPerpVec2(axis))
//        }
      })
    }
  )
}

class GraphAxisTitle(val axis: Axis, name: BoxScript[String]) extends UnboundedGraphDisplayLayer {
  def paint = for {
    currentName <- name
  } yield {
    (canvas:GraphCanvas) => {
      val a = canvas.spaces.pixelArea
      val tl = a.origin + a.size.withX(0)
      val br = a.origin + a.size.withY(0)

      canvas.color = GraphAxis.fontColor
      canvas.fontSize = GraphAxis.titleFontSize
      axis match {
        case X => canvas.string(currentName, br + Vec2(-10, 28), Vec2(1, 1))
        case Y => canvas.string(currentName, tl + Vec2(-52, 10 ), Vec2(1, 0), -1)
      }
    }
  }
}

trait GraphBoxAction {
  def apply(area: Area, spaces: GraphSpaces): BoxScript[Unit]
}

// class GraphBox(fill: Box[Color], outline: Box[Color], enabled: Box[Boolean], action: GraphBoxAction, val minSize:Int = 5, val axis: Option[Axis] = None)(implicit shelf: Shelf) extends GraphLayer {
//   private val area: Box[Option[Area]] = BoxNow(None)

//   def bigEnough(a:Area) = (math.abs(a.size.x) > minSize || math.abs(a.size.y) > minSize)

//   def paint(implicit txn: TxnR) = {
//     val cFill = fill()
//     val cOutline = outline()
//     val cEnabled = enabled()
//     val cArea = area()

//     (canvas:GraphCanvas) => {
//       if (cEnabled) {
//         cArea.foreach(a => {
//           val pixelArea = canvas.spaces.toPixel(a)
//           if (bigEnough(pixelArea)) {
//             canvas.color = cFill
//             canvas.fillRect(canvas.spaces.toPixel(a))
//             canvas.color = cOutline
//             canvas.drawRect(canvas.spaces.toPixel(a))
//           }
//         })
//       }
//     }
//   }

//   def onMouse(e:GraphMouseEvent)(implicit txn: Txn) = {
//     if (enabled()) {
//       e.eventType match {
//         case PRESS => {
//           area() = Some(Area(e.dataPoint, Vec2(0, 0)).replaceAxis(axis, e.spaces.dataArea))
//           true
//         }
//         case DRAG => {
//           area().foreach(a => {
//             area() = Some(Area(a.origin, e.dataPoint - a.origin).replaceAxis(axis, e.spaces.dataArea))
//           })
//           true
//         }
        
//         case RELEASE => {
//           area().foreach(a => {
//             area() = None
//             val dragArea = Area(a.origin, e.dataPoint - a.origin)
//             val pixelDragArea = e.spaces.toPixel(dragArea)
//             if (bigEnough(pixelDragArea)) {
//               action.apply(dragArea.replaceAxis(axis, e.spaces.dataArea), e.spaces)
//             }
//           })
//           true
//         }
//         case _ => false
//       }
//     } else {
//       false
//     }

//   }

//   val dataBounds = BoxNow(None:Option[Area])

// }

// object GraphSelectBox {

//   def curvePointInArea(curve:List[Vec2], area:Area) = {
//     curve.foldLeft(false) {
//       (contains, p) => contains || area.contains(p)
//     }
//   }

//   def curveIntersectsArea(curve:List[Vec2], area:Area) = {
//     val rect = new Rectangle2D.Double(area.origin.x, area.origin.y, area.size.x, area.size.y)

//     //TODO we should finish this early if possible - there is some way to do this
//     val result = curve.foldLeft((false, None:Option[Vec2])){
//       (result, current) => {
//         val intersects = result._1
//         val previous = result._2
//         if (intersects) {
//           (intersects, Some(current))
//         } else {
//           previous match {
//             case None => (false, Some(current))
//             case Some(p) => {
//               if (rect.intersectsLine(p.x, p.y, current.x, current.y)) {
//                 (true, Some(current))
//               } else {
//                 (false, Some(current))
//               }
//             }
//           }
//         }
//       }
//     }

//     result._1
//   }

//   def seriesSelected(series:Series[_], area:Area) = {
//     if (series.painter.linesDrawn(series)) {
//       curveIntersectsArea(series.curve, area)
//     } else {
//       curvePointInArea(series.curve, area)
//     }
//   }


//   def apply[K](series:Box[List[Series[K]]], fill:Box[Color], outline:Box[Color], selectionOut:Box[Set[K]], enabled:Box[Boolean])(implicit shelf: Shelf) = {
    
//     val action = new GraphBoxAction {
//       def apply(area: Area, spaces: GraphSpaces)(implicit txn: Txn) {
//         val areaN = area.normalise
//         val selected = series().collect{
//           case s if (seriesSelected(s, areaN)) => s.key
//         }
//         selectionOut() = selected.toSet        
//       }
//     }
    
//     new GraphBox(fill, outline, enabled, action)
//   }
// }

// object GraphZoomBox {
//   def apply(fill: Box[Color], outline: Box[Color], areaOut: Box[Option[Area]], enabled: Box[Boolean])(implicit shelf: Shelf) = {
//     val action = new GraphBoxAction {
//       def apply(area: Area, spaces: GraphSpaces)(implicit txn: Txn) {
//         //Zoom out for second quadrant drag (x negative, y positive)
//         if (area.size.x < 0 && area.size.y > 0) {
//           areaOut() = None
//         } else {
//           areaOut() = Some(area.normalise)
//         }
//       }
//     }
    
//     new GraphBox(fill, outline, enabled, action)
//   }
// }


// object GraphGrab{
//   def apply(enabled: Box[Boolean], manualDataArea: Box[Option[Area]], displayedDataArea: Box[Area])(implicit shelf: Shelf) = new GraphGrab(enabled, manualDataArea, displayedDataArea)
// }

// class GraphGrab(enabled:Box[Boolean], manualDataArea:Box[Option[Area]], displayedDataArea:Box[Area])(implicit shelf: Shelf) extends GraphLayer {

//   private val maybeInitial: Box[Option[GraphMouseEvent]] = BoxNow(None)

//   def paint(implicit txn: TxnR) = (canvas:GraphCanvas) => {}

//   def onMouse(current:GraphMouseEvent)(implicit txn: Txn) = {
//     if (enabled()) {
//       current.eventType match {
//         case PRESS => {
//           maybeInitial() = Some(current)
//           true
//         }
//         case DRAG => {
//           maybeInitial().foreach(initial => {
//             //If there is no manual zoom area, set it to the displayed area
//             if (manualDataArea() == None) {
//               manualDataArea() = Some(displayedDataArea())
//             }
//             manualDataArea().foreach(a => {
//               val initialArea = initial.spaces.dataArea
//               val currentPixelOnInitialArea = initial.spaces.toData(current.spaces.toPixel(current.dataPoint))

//               val dataDrag = initial.dataPoint - currentPixelOnInitialArea
//               manualDataArea() = Some(Area(initialArea.origin + dataDrag, initialArea.size))
//             })
//           })
//           true
//         }
//         case RELEASE => {
//           maybeInitial() = None
//           true
//         }
//         case _ => false
//       }
//     } else {
//       false
//     }
//   }

//   val dataBounds = BoxNow(None:Option[Area])

// }

// object AxisTooltip {
//   val format = new DecimalFormat("0.0000")
//   def apply(axis:Axis, enabled:Box[Boolean])(implicit shelf: Shelf) = new AxisTooltip(axis, enabled)
//   val horizTabPainter = new GraphThreePartPainter(IconFactory.image("HorizontalLineLabel"))
//   val vertTabPainter = new GraphThreePartPainterVertical(IconFactory.image("VerticalLineLabel"))
//   val lineColor = SwingView.shadedBoxColor

//   def drawAxisLine(canvas:GraphCanvas, v:Double, a:Axis, label:String, color:Option[Color]) = {
//     canvas.clipToData()
//     val dataArea = canvas.spaces.dataArea
//     val start = canvas.spaces.toPixel(dataArea.axisPosition(a, v))
//     val end = start + canvas.spaces.pixelArea.axisPerpVec2(a)

//     canvas.lineWidth = 1
//     canvas.color = color.getOrElse(AxisTooltip.lineColor)
//     canvas.line(start, end)

//     canvas.color = GraphAxis.fontColor
//     canvas.fontSize = GraphAxis.fontSize

//     val size = canvas.stringSize(label)

//     val colorOffset = if (color == None) 0 else 12;

//     //TODO combine code
//     a match {
//       case X => {
//         AxisTooltip.vertTabPainter.paint(canvas, start + Vec2(-16, -4 - 23 - size.x - colorOffset), Vec2(16, size.x + 23 + colorOffset))
//         canvas.color = SwingView.selectedTextColor
//         canvas.string(label, start + Vec2(-3, -15 - colorOffset), Vec2(0, 0), -1)
//         color.foreach(c => {
//           val swatch = Area(start + Vec2(-11, -21), Vec2(7, 7))
//           canvas.color = c
//           canvas.fillRect(swatch)
//           canvas.color = SwingView.selectedTextColor
//           canvas.drawRect(swatch)
//         })
//       }
//       case Y => {
//         AxisTooltip.horizTabPainter.paint(canvas, start + Vec2(4, -16), Vec2(size.x + 23 + colorOffset, 16))
//         canvas.color = SwingView.selectedTextColor
//         canvas.string(label, start + Vec2(15 + colorOffset, -3), Vec2(0, 0), 0)
//         color.foreach(c => {
//           val swatch = Area(start + Vec2(14, -11), Vec2(7, 7))
//           canvas.color = c
//           canvas.fillRect(swatch)
//           canvas.color = SwingView.selectedTextColor
//           canvas.drawRect(swatch)
//         })
//       }
//     }

//     size.x + 23 + 4 + colorOffset

//   }

// }

// class AxisTooltip(axis:Axis, enabled:Box[Boolean])(implicit shelf: Shelf) extends GraphLayer {

//   private val value: Box[Option[Double]] = BoxNow(None)

//   def paint(implicit txn: TxnR) = {

//     val a = axis
    
//     val maybeV = value()
//     val e = enabled()

//     (canvas:GraphCanvas) => {
//       if (e) {
//         maybeV.foreach(v => {
//           val label = AxisTooltip.format.format(v)
//           AxisTooltip.drawAxisLine(canvas, v, a, label, None)
//         })
//       }
//     }
//   }

//   def onMouse(e:GraphMouseEvent)(implicit txn: Txn) = {
//     if (enabled()) {
//       e.eventType match {
//         case MOVE => {
//           val axisPosition = e.spaces.pixelArea.axisRelativePosition(Axis.other(axis), e.spaces.toPixel(e.dataPoint)) * (if (axis == X) -1 else 1)
//           if (axisPosition <= 0 && axisPosition > -32) {
//             value() = Some(e.dataPoint.onAxis(axis))
//           } else {
//             value() = None
//           }
//         }
//         case _ => value() = None
//       }
//       false
//     } else {
//       false
//     }

//   }

//   val dataBounds = BoxNow(None:Option[Area])

// }

// class GraphZoomer(
//     val dataBounds: Box[Option[Area]],
//     val manualBounds: Box[Option[Area]],
//     val xAxis: Box[GraphZoomerAxis],
//     val yAxis: Box[GraphZoomerAxis])(implicit shelf: Shelf) {

//   def autoArea(implicit txn: Txn) = {
//     dataBounds() match {
//       case None => {
//         //We have no data bounds, so use the axes required ranges,
//         //or 0 to 1 in each axis if there are none.
//         val xRange = xAxis().requiredRange().getOrElse((0d, 1d))
//         val yRange = yAxis().requiredRange().getOrElse((0d, 1d))
//         Area(Vec2(xRange._1, yRange._1), Vec2(xRange._2, yRange._2)).normalise
//       }
//       case Some(area) => {
//         //We have a data bounds area, so pad it appropriately
//         val auto = area.pad(Vec2(xAxis().paddingBefore(), yAxis().paddingBefore()), Vec2(xAxis().paddingAfter(), yAxis().paddingAfter()))

//         val padX = xAxis().requiredRange().foldLeft(auto){(area, range) => area.extendToContain(Vec2(range._1, auto.origin.y)).extendToContain(Vec2(range._2, auto.origin.y))}
//         val padY = yAxis().requiredRange().foldLeft(padX){(area, range) => area.extendToContain(Vec2(auto.origin.x, range._1)).extendToContain(Vec2(auto.origin.x, range._2))}

//         padY
//       }
//     }
//   }

//   val dataArea = BoxNow.calc(implicit txn => {
//     //Use manual bounds if specified, automatic area from data bounds etc.
//     //Make sure that size is at least the minimum for each axis
//     val a = manualBounds().getOrElse(autoArea)
//     a.sizeAtLeast(Vec2(xAxis().minSize(), yAxis().minSize()))
//   })
// }

// object GraphClickToSelectSeries{
//   def apply[K](series: Box[List[Series[K]]], selectionOut: Box[Set[K]], enabled: Box[Boolean])(implicit shelf: Shelf) = new GraphClickToSelectSeries(series, selectionOut, enabled)
// }

// class GraphClickToSelectSeries[K](series: Box[List[Series[K]]], selectionOut: Box[Set[K]], enabled: Box[Boolean])(implicit shelf: Shelf) extends GraphLayer {

//   def paint(implicit txn: TxnR) = (canvas:GraphCanvas) => {}

//   def onMouse(e:GraphMouseEvent)(implicit txn: Txn) = {
//     if (enabled()) {
//       e.eventType match {
//         case CLICK => {
//           val selectedSeries = SeriesSelection.selectedSeries(series(), e)
//           selectedSeries.foreach((ss) => selectionOut() = Set(ss.key))
//           selectedSeries.isDefined
//         }
//         case _ => false
//       }
//     } else {
//       false
//     }      
//   }

//   val dataBounds = BoxNow(None:Option[Area])

// }

// case class GraphBasic(layers: Box[List[GraphLayer]], overlayers: Box[List[GraphLayer]], dataArea: Box[Area], borders: Box[Borders], highQuality: Box[Boolean])(implicit shelf: Shelf) extends Graph {}

