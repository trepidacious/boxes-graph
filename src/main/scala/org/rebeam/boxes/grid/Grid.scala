package org.rebeam.boxes.grid

import org.rebeam.boxes.core._
import BoxScriptImports._
import BoxTypes._
import BoxUtils._

import org.rebeam.boxes.graph._
import Axis._

import scalaz._
import Scalaz._

case class Column[A](name: BoxR[String], list: BoxR[List[A]])

trait CellPainter[-A] {
	def paint(a: A, origin: Vec2, cellSize: Vec2): BoxScript[GraphCanvas => Unit]
}

class CellPainterString[A](toString: A => String) extends CellPainter[A] {
	val align = Vec2(0, 1)
	def paint(a: A, origin: Vec2, cellSize: Vec2): BoxScript[GraphCanvas => Unit] = {
		just((c: GraphCanvas) => {
			c.color = java.awt.Color.BLACK
			//TODO align string, and/or use ellipsis if it is too long, or clip
			val originPixel = c.spaces.toPixel(origin)
			c.string(toString(a), originPixel, align)
		})
	}
}

//TODO painter using colour and string, drawing a swatch of the color.

object CellPainterDefault extends CellPainterString((a: Any) => a.toString)

class GridColumn[A](
	column: BoxR[Column[A]], 
	origin: BoxR[Vec2], 
	cellSize: BoxR[Vec2], 
	visibleArea: BoxR[Area], 
	cellPainter: CellPainter[A]
) extends GraphDisplayLayer {

	def visibleCellsAndPositions(list: List[A], visibleArea: Area, cellSize: Vec2, origin: Vec2): List[(A, Vec2)] = {
		val firstCellArea = Area(origin, cellSize)
		//First check that we have some x-intersection - otherwise nothing is visible
		if (!firstCellArea.axisIntersects(X, visibleArea)) {
			Nil
		//Now check in the y axis
		} else {
			//Find first at least partially visible cell, and first completely invisible cell (last)
			val first = math.max(0, math.floor((origin.y - visibleArea.top)/cellSize.y).toInt)
			val last = math.min(list.size, math.ceil((origin.y - visibleArea.bottom)/cellSize.y).toInt)
			//View the visible elements, if any
			if (first <= last) {
				list.zipWithIndex.map{case (a, i) => (a, origin + Vec2(0, -cellSize.y * i))}.slice(first, last)
			} else {
				Nil
			}
		}
	}
		
	def paint = for {
		c <- column
		n <- c.name
		l <- c.list
		o <- origin
		s <- cellSize
		va <- visibleArea
		vcap = visibleCellsAndPositions(l, va, s, o)
		draws <- vcap traverseU (a => cellPainter.paint(a._1, a._2, s))
	} yield {
		(canvas: GraphCanvas) => {
			canvas.clipToData
			
			//TODO any over-all drawing of column (e.g. dividers)
			
			//Execute the cell draws
			draws.foreach(d => d(canvas))
		}
	}
	
	val viewRegion = for {
		c <- column
		l <- c.list
		o <- origin
		s <- cellSize
	} yield {
		RegionXY(Some(Area(o, Vec2(s.x, s.y * l.size))))
	}
}
