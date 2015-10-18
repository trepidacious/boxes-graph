package org.rebeam.boxes.graph.swing.demo

import org.rebeam.boxes.swing._
import org.rebeam.boxes.swing.views._
import org.rebeam.boxes.graph._
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

object GraphSwingDemoApp extends App {

  SwingView.later {
    SwingView.nimbus

    val series1 = new Series("Key 1", List(Vec2(0,0), Vec2(1,1)), Color.blue, 2)
    val series2 = new Series("Key 2", List(Vec2(0,1), Vec2(1,0)), Color.red, 2)
    
    val selection = atomic { create(Set.empty[String]) }

    val x = atomic { create(-0.5d) }
    val xThreshold = GraphThreshold(just(X), x, just(Color.blue), just("X Threshold"), just(true))

    val y = atomic { create(-0.5d) }
    val yThreshold = GraphThreshold(just(Y), y, just(Color.red), just("Y Threshold"), just(true))

    val series = just(List(series1, series2))

    val seriesBySelection = ColorSeriesBySelection(series, selection)

    val graph = Charts.withSeries(
        series = seriesBySelection,
        // series = series,
        manualBounds = atomic { create(None: Option[Area]) },
        selection = selection,
        zoomEnabled = just(false),
        grabEnabled = just(true),
        extraOverLayers = List(xThreshold, yThreshold)
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