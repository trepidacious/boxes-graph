package org.rebeam.boxes.graph

import org.rebeam.boxes.core._
import org.rebeam.boxes.swing.SwingView
import org.rebeam.boxes.swing.icons.IconFactory
import BoxScriptImports._
import BoxTypes._
import BoxUtils._

import java.awt.Color
import java.awt.geom.Rectangle2D
import java.text.DecimalFormat

import GraphMouseEventType._
import Axis._

trait Graph {
  def layers: BoxR[List[GraphLayer]]
  def overlayers: BoxR[List[GraphLayer]]
  def dataArea: BoxR[Area]
  def borders: BoxR[Borders]
  def highQuality: BoxR[Boolean]
}

case class GraphBasic (
  layers: BoxR[List[GraphLayer]], 
  overlayers: BoxR[List[GraphLayer]], 
  dataArea: BoxR[Area], 
  borders: BoxR[Borders], 
  highQuality: BoxR[Boolean]) extends Graph

case class GraphZoomerAxis(
  requiredRange: BoxR[Interval],
  paddingBefore: BoxR[Double],
  paddingAfter: BoxR[Double],
  minSize: BoxR[Double]
)

object GraphDefaults {
  def axis = GraphZoomerAxis(just(Interval.all), just(0.05), just(0.05), just(0.01))
  def axis(paddingBefore: Double, paddingAfter: Double) = GraphZoomerAxis(just(Interval.all), just(paddingBefore), just(paddingAfter), just(0.01))
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

class GraphBusy(val alpha: BoxR[Double]) {//extends UnboundedGraphDisplayLayer {
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

class GraphAxisTitle(val axis: Axis, name: BoxR[String]) extends UnboundedGraphDisplayLayer {
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

class GraphBox(fill: BoxR[Color], outline: BoxR[Color], enabled: BoxR[Boolean], action: GraphBoxAction, val minSize: Int = 5, val axis: Option[Axis] = None) extends UnboundedGraphLayer {
  private val area = atomic { create(None: Option[Area]) }

  def bigEnough(a: Area) = (math.abs(a.size.x) > minSize || math.abs(a.size.y) > minSize)

  def paint = for {
    cFill <- fill
    cOutline <- outline
    cEnabled <- enabled
    cArea <- area()
  } yield {
    (canvas:GraphCanvas) => {
      if (cEnabled) {
        cArea.foreach(a => {
          val pixelArea = canvas.spaces.toPixel(a)
          if (bigEnough(pixelArea)) {
            canvas.color = cFill
            canvas.fillRect(canvas.spaces.toPixel(a))
            canvas.color = cOutline
            canvas.drawRect(canvas.spaces.toPixel(a))
          }
        })
      }
    }
  }

  private def doRelease(a: Area, e: GraphMouseEvent): BoxScript[Unit] = {
    val dragArea = Area(a.origin, e.dataPoint - a.origin)
    val pixelDragArea = e.spaces.toPixel(dragArea)
    if (bigEnough(pixelDragArea)) {
      action.apply(dragArea.replaceAxis(axis, e.spaces.dataArea), e.spaces)
    } else {
      nothing
    }
  }

  def onMouse(e: GraphMouseEvent) = for {
    en <- enabled
    consumed <- if (en) {
      e.eventType match {
        
        case Press => (area() = Some(Area(e.dataPoint, Vec2(0, 0)).replaceAxis(axis, e.spaces.dataArea))) andThen just(true)

        //Modify selected area. Note that this is Option[Area], so we use map to do this 
        case Drag => area.modify(_.map(a => Area(a.origin, e.dataPoint - a.origin).replaceAxis(axis, e.spaces.dataArea))) andThen just(true)

        case Release => for {
          maybeA <- area.modify(_ => None)
          //FIXME can we make this neater?
          _ <- maybeA.map(a => doRelease(a, e)).getOrElse(nothing)
        } yield true

        case _ => just(false)
      }
    } else {
      just(false)
    }
  } yield consumed

}

object GraphSelectBox {

  def curvePointInArea(curve: List[Vec2], area: Area) = {
    curve.foldLeft(false) {
      (contains, p) => contains || area.contains(p)
    }
  }

  def curveIntersectsArea(curve:List[Vec2], area:Area) = {
    val rect = new Rectangle2D.Double(area.origin.x, area.origin.y, area.size.x, area.size.y)

    //TODO we should finish this early if possible - there is some way to do this
    val result = curve.foldLeft((false, None: Option[Vec2])){
      (result, current) => {
        val intersects = result._1
        val previous = result._2
        if (intersects) {
          (intersects, Some(current))
        } else {
          previous match {
            case None => (false, Some(current))
            case Some(p) => {
              if (rect.intersectsLine(p.x, p.y, current.x, current.y)) {
                (true, Some(current))
              } else {
                (false, Some(current))
              }
            }
          }
        }
      }
    }

    result._1
  }

  def seriesSelected(series:Series[_], area:Area) = {
    if (series.painter.linesDrawn(series)) {
      curveIntersectsArea(series.curve, area)
    } else {
      curvePointInArea(series.curve, area)
    }
  }


  def apply[K](series: BoxR[List[Series[K]]], fill: BoxR[Color], outline: BoxR[Color], selectionOut: BoxM[Set[K]], enabled: BoxR[Boolean]) = {
    
    val action = new GraphBoxAction {
      def apply(area: Area, spaces: GraphSpaces) = {
        val areaN = area.normalise
        for {
          s <- series
          selected = s.collect{
            case s if (seriesSelected(s, areaN)) => s.key
          }
          _ <- selectionOut() = selected.toSet
        } yield ()
      }
    }
    
    new GraphBox(fill, outline, enabled, action)
  }
}

object GraphZoomBox {
  def apply(fill: BoxR[Color], outline: BoxR[Color], areaOut: BoxM[Option[Area]], enabled: BoxR[Boolean]) = {
    val action = new GraphBoxAction {
      def apply(area: Area, spaces: GraphSpaces) = {
        //Zoom out for second quadrant drag (x negative, y positive)
        if (area.size.x < 0 && area.size.y > 0) {
          areaOut() = None
        } else {
          areaOut() = Some(area.normalise)
        }
      }
    }
    
    new GraphBox(fill, outline, enabled, action)
  }
}

class GraphZoomer(
    val viewRegion: BoxR[RegionXY],
    val manualBounds: BoxR[Option[Area]],
    val xAxis: BoxR[GraphZoomerAxis],
    val yAxis: BoxR[GraphZoomerAxis]) {

  def autoArea = for {
    vr <- viewRegion

    xa <- xAxis
    ya <- yAxis
    
    xar <- xa.requiredRange
    yar <- ya.requiredRange

    xapb <- xa.paddingBefore
    yapb <- ya.paddingBefore

    xapa <- xa.paddingAfter
    yapa <- ya.paddingAfter

  } yield {
    //Enforce axis required ranges in the same way as a graph layer viewRegion
    val vrRequired = vr.definedOuter(RegionXY(xar, yar))

    //Make into a finite area. For each axis, if we don't have a size already, use 1.
    val area = vrRequired.toArea(1, 1)

    //Finally pad the axes
    area.pad(Vec2(xapb, yapb), Vec2(xapa, yapa))
  }

  val dataArea = for {
    mb <- manualBounds
    aa <- autoArea
    xam <- xAxis.flatMap(_.minSize)
    yam <- yAxis.flatMap(_.minSize)
  } yield {
    //Use manual bounds if specified, automatic area from data bounds etc.
    //Make sure that size is at least the minimum for each axis
    mb.getOrElse(aa).sizeAtLeast(Vec2(xam, yam))
  }

}

object GraphGrab{
  def apply(enabled: BoxR[Boolean], manualDataArea: BoxM[Option[Area]], displayedDataArea: BoxR[Area]) = new GraphGrab(enabled, manualDataArea, displayedDataArea)
}

class GraphGrab(enabled: BoxR[Boolean], manualDataArea: BoxM[Option[Area]], displayedDataArea: BoxR[Area]) extends UnboundedInputGraphLayer {

  private val maybeInitial = atomic { create(None: Option[GraphMouseEvent]) }

  def onMouse(current: GraphMouseEvent): BoxScript[Boolean] = for {
    en <- enabled
    mi <- maybeInitial()
    mda <- manualDataArea()
    dda <- displayedDataArea
    consumed <- if (en) {
      current.eventType match {
        case Press => (maybeInitial() = Some(current)) andThen just(true)

        case Drag => mi.map { 
          initial => {
            for {
              _ <- if (mda == None) manualDataArea() = Some(dda) else nothing
              mda2 <- manualDataArea()
              
              _ <- mda2.map{
                da => {
                  val initialArea = initial.spaces.dataArea
                  val currentPixelOnInitialArea = initial.spaces.toData(current.spaces.toPixel(current.dataPoint))
                  val dataDrag = initial.dataPoint - currentPixelOnInitialArea
                  manualDataArea() = Some(Area(initialArea.origin + dataDrag, initialArea.size))
                }
              }.getOrElse(nothing)

            } yield (true)
          }
        }.getOrElse(just(true))

        case Release => (maybeInitial() = None) andThen just(true)

        case _ => just(false)
      }

    } else {
      just(false)
    }
  } yield consumed

}

object GraphClickToSelectSeries{
  def apply[K](series: BoxR[List[Series[K]]], selectionOut: BoxM[Set[K]], enabled: BoxR[Boolean]) = new GraphClickToSelectSeries(series, selectionOut, enabled)
}

class GraphClickToSelectSeries[K](series: BoxR[List[Series[K]]], selectionOut: BoxM[Set[K]], enabled: BoxR[Boolean]) extends UnboundedInputGraphLayer {

  def onMouse(e: GraphMouseEvent): BoxScript[Boolean] = for {
    en <- enabled
    s <- series
    consumed <- if (en) {
      e.eventType match {
        case Click => {
          val selectedSeries = SeriesSelection.selectedSeries(s, e)
          selectedSeries.map(ss => selectionOut() = Set(ss.key)).getOrElse(nothing) andThen just(selectedSeries.isDefined)
        }
        case _ => just(false)
      }
    } else {
      just(false)      
    }
  } yield consumed

}

object AxisTooltip {

  val format = new DecimalFormat("0.0000")
  val horizTabPainter = new GraphThreePartPainter(IconFactory.image("HorizontalLineLabel"))
  val vertTabPainter = new GraphThreePartPainterVertical(IconFactory.image("VerticalLineLabel"))
  val lineColor = SwingView.shadedBoxColor

  def drawAxisLine(canvas: GraphCanvas, v: Double, a: Axis, label: String, color: Option[Color]) = {
    canvas.clipToData()
    val dataArea = canvas.spaces.dataArea
    val start = canvas.spaces.toPixel(dataArea.axisPosition(a, v))
    val end = start + canvas.spaces.pixelArea.axisPerpVec2(a)

    canvas.lineWidth = 1
    canvas.color = color.getOrElse(AxisTooltip.lineColor)
    canvas.line(start, end)

    canvas.color = GraphAxis.fontColor
    canvas.fontSize = GraphAxis.fontSize

    val size = canvas.stringSize(label)

    val colorOffset = if (color == None) 0 else 12;

    //TODO combine code
    a match {
      case X => {
        AxisTooltip.vertTabPainter.paint(canvas, start + Vec2(-16, -4 - 23 - size.x - colorOffset), Vec2(16, size.x + 23 + colorOffset))
        canvas.color = SwingView.selectedTextColor
        canvas.string(label, start + Vec2(-3, -15 - colorOffset), Vec2(0, 0), -1)
        color.foreach(c => {
          val swatch = Area(start + Vec2(-11, -21), Vec2(7, 7))
          canvas.color = c
          canvas.fillRect(swatch)
          canvas.color = SwingView.selectedTextColor
          canvas.drawRect(swatch)
        })
      }
      case Y => {
        AxisTooltip.horizTabPainter.paint(canvas, start + Vec2(4, -16), Vec2(size.x + 23 + colorOffset, 16))
        canvas.color = SwingView.selectedTextColor
        canvas.string(label, start + Vec2(15 + colorOffset, -3), Vec2(0, 0), 0)
        color.foreach(c => {
          val swatch = Area(start + Vec2(14, -11), Vec2(7, 7))
          canvas.color = c
          canvas.fillRect(swatch)
          canvas.color = SwingView.selectedTextColor
          canvas.drawRect(swatch)
        })
      }
    }

    size.x + 23 + 4 + colorOffset

  }

  def apply(axis: Axis, enabled: BoxR[Boolean]) = new AxisTooltip(axis, enabled)

}

class AxisTooltip(axis: Axis, enabled: BoxR[Boolean]) extends UnboundedGraphLayer {

  private val value = atomic { create(None: Option[Double]) }

  def paint = for {
    maybeV <- value()
    e <- enabled
  } yield {
    (canvas:GraphCanvas) => {
      if (e) {
        maybeV.foreach(v => {
          val label = AxisTooltip.format.format(v)
          AxisTooltip.drawAxisLine(canvas, v, axis, label, None)
        })
      }
    }
  }

  def onMouse(e: GraphMouseEvent) = for {
    en <- enabled
    _ <- if (en) {
      e.eventType match {
        case Move => {
          val axisPosition = e.spaces.pixelArea.axisRelativePosition(Axis.other(axis), e.spaces.toPixel(e.dataPoint)) * (if (axis == X) -1 else 1)
          if (axisPosition <= 0 && axisPosition > -32) {
            value() = Some(e.dataPoint.onAxis(axis))
          } else {
            value() = None
          }
        }
        case _ => (value() = None)
      }
    } else {
      nothing
    }
  } yield false
  
}


