package org.rebeam.boxes.graph.swing.demo

import org.rebeam.boxes.swing._
import org.rebeam.boxes.swing.views._
import org.rebeam.boxes.graph._
import org.rebeam.boxes.grid._
import org.rebeam.boxes.graph.swing._
import org.rebeam.boxes.core._


import java.awt.Color
import javax.swing.{JFrame, JPanel}
import java.awt.BorderLayout
import java.awt.Dimension

import BoxTypes._
import BoxScriptImports._
import BoxUtils._

import Axis._

import scala.math._

object GridSwingDemoApp extends App {

  SwingView.later {
    SwingView.nimbus

    val series = just(List.empty[Series[String]])
    
    val selection = atomic { create(Set.empty[String]) }

    val list = List("Alicia", "Bobo", "Charlemagne")
    val column = Column(just("Names"), just(list))
    val gridColumn = new GridColumn(just(column), just(Vec2(0, 1)), just(Vec2(0.5, 0.1)), just(Area(Vec2.zero, Vec2(1, 1))), CellPainterDefault)

    val seriesBySelection = ColorSeriesBySelection(series, selection)

    val graph = Charts.withSeries(
        series = seriesBySelection,
        manualBounds = atomic { create(None: Option[Area]) },
        selection = selection,
        zoomEnabled = just(false),
        grabEnabled = just(true),
        extraOverLayers = List(gridColumn)
        // highQuality = just(false)
    )
    
    val selectionString = selection().map("Selection: " + _.toList.mkString)

    val selectionLabel = LabelView(selectionString)

    val view = GraphSwingView(just(graph))
    
    val frame = new JFrame("Graph Swing Demo")

    val panel = new JPanel(new BorderLayout())
    panel.add(view.component, BorderLayout.CENTER)
    panel.add(selectionLabel.component, BorderLayout.SOUTH)

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    view.component.setMinimumSize(new Dimension(400, 400))
    view.component.setPreferredSize(new Dimension(400, 400))
    view.component.setSize(new Dimension(400, 400))
    frame.add(panel)
    frame.pack()
    frame.setVisible(true)
  }

}