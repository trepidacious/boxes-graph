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

import java.awt.Color
import java.text.DecimalFormat
import java.util.concurrent.atomic.AtomicReference

object GraphThreshold {
  val format = new DecimalFormat("0.00")
  val handleRadius = 3

  def apply(axis: BoxR[Axis], value: Box[Double], color: BoxR[Color], name: BoxR[String], enabled: BoxR[Boolean]) = new GraphThreshold(axis, value, color, name, enabled)
}

class GraphThreshold(axis: BoxR[Axis], value: Box[Double], color: BoxR[Color], name: BoxR[String], enabled: BoxR[Boolean]) extends GraphLayer {

  private val labelWidth = new AtomicReference[Double](0d)

  def paint = for {
    c <- color
    a <- axis
    v <- value()
    n <- name
  } yield {
    (canvas:GraphCanvas) => {
      val label = n + ": " + GraphThreshold.format.format(v)
      labelWidth.set(AxisTooltip.drawAxisLine(canvas, v, a, label, Some(c)))
    }
  }

  private val pressOffset: Box[Option[Double]] = atomic { create(None: Option[Double]) }

  def dataPointValue(point: Vec2) = axis.map(_ match {
    case X => point.x
    case Y => point.y
  })
  
  def onMouse(e: GraphMouseEvent): BoxScript[Boolean] = for {
    en <- enabled
    dpv <- dataPointValue(e.dataPoint)
    v <- value()
    a <- axis
    po <- pressOffset()
    pixelPoint = e.spaces.toPixel(e.dataPoint)
    valuePoint = e.spaces.toPixel(e.spaces.dataArea.axisPosition(a, v))
    pixelPerpDistance = (valuePoint - pixelPoint).onAxis(a)
    pixelDistance = (valuePoint - pixelPoint).onAxis(Axis.other(a)) * (if (a == X) 1 else -1)
    insideHandle = ((pixelPerpDistance > -2 && pixelPerpDistance < 18 && pixelDistance > 0 && pixelDistance < labelWidth.get()) || math.abs(pixelPerpDistance) < GraphThreshold.handleRadius)

    consumed <- if (en) {
      e.eventType match {
        case Click => just(insideHandle)  //Consume clicks in handle - may use later

        case Press => if (insideHandle) {
          (pressOffset() = Some(v - dpv)) andThen just(true)
        } else {
          just(false)
        }

        case Drag => po match {
          case Some(offset) => {
            (value() = dpv + offset) andThen just(true)
          }
          case None => just(false)
        }

        case Release => po match {
          
          case Some(offset) => for {
            _ <- value() = dpv + offset
            _ <- pressOffset() = None            
          } yield true

          case None => just(false)
        }

        case _ => just(false)

      }
    } else {
      just(false)      
    }

  } yield consumed

  //   if (enabled()) {
  //     val pixelPoint = e.spaces.toPixel(e.dataPoint)
  //     val valuePoint = e.spaces.toPixel(e.spaces.dataArea.axisPosition(axis(), value()))
  //     val pixelPerpDistance = (valuePoint - pixelPoint).onAxis(axis())
  //     val pixelDistance = (valuePoint - pixelPoint).onAxis(Axis.other(axis())) * (if (axis() == X) 1 else -1)
  //     val insideHandle = ((pixelPerpDistance > -2 && pixelPerpDistance < 18 && pixelDistance > 0 && pixelDistance < labelWidth.get()) || math.abs(pixelPerpDistance) < GraphThreshold.handleRadius)
  //     e.eventType match {
  //       case CLICK => insideHandle  //Consume clicks in handle - may use later
  //       case PRESS => {
  //         if (insideHandle) {
  //           pressOffset = Some(value() - dpv)
  //           true
  //         } else {
  //           false
  //         }
  //       }
  //       case DRAG => pressOffset match {
  //         case Some(offset) => {
  //           value() = dpv + offset
  //           true
  //         }
  //         case None => false
  //       }
  //       case RELEASE => pressOffset match {
  //         case Some(offset) => {
  //           value() = dpv + offset
  //           pressOffset = None
  //           true
  //         }
  //         case None => false
  //       }
  //       case _ => false
  //     }
  //   } else {
  //     false
  //   }
  // }

  val viewRegion = for {
    a <- axis
    v <- value()
  } yield a match {
    case X => RegionXY.xEqualTo(v)
    case Y => RegionXY.yEqualTo(v)
  }

}