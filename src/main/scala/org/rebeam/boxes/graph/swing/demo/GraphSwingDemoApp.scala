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

import scala.math._

object GraphSwingDemoApp extends App {

  SwingView.later {
    SwingView.nimbus

    val pointCount = 100
    val count = 30
    val countD = count * 1.0d
    val series = just(Range(0, count).toList.map(i => {
      val w = Range.Double(0.0, 1.0, 1.0d/pointCount).toList.map(x => Vec2(x, (i/countD) + (countD/2.0 - i)/(countD/2.0) * x + 0.1*sin(x*9.3)))      
      new Series("Key " + i, w, if (i%2==0) java.awt.Color.blue else java.awt.Color.red, 1)
    }))

    // val w1 = Range.Double(0.0, 1.0, 0.001).toList.map(x => Vec2(x, x + 0.1*sin(x*9.3)))
  	// val w2 = Range.Double(0.0, 1.0, 0.001).toList.map(x => Vec2(x, 1 - (x + 0.1*sin(x*9.3))))
    // 
  	// val series1 = new Series("Key 1", w1, java.awt.Color.blue, 2)
  	// val series2 = new Series("Key 2", w2, java.awt.Color.red, 2)
    // val series = just(List(series1, series2))
    
    val selection = atomic { create(Set.empty[String]) }

    val x = atomic { create(0.5d) }
    val xThreshold = GraphThreshold(just(X), x, just(Color.blue), just("X Threshold"), just(true))

    val y = atomic { create(0.5d) }
    val yThreshold = GraphThreshold(just(Y), y, just(Color.red), just("Y Threshold"), just(true))

    val seriesBySelection = ColorSeriesBySelection(series, selection)

    val graph = Charts.withSeries(
        series = seriesBySelection,
        manualBounds = atomic { create(None: Option[Area]) },
        selection = selection,
        zoomEnabled = just(false),
        grabEnabled = just(true),
        extraOverLayers = List(xThreshold, yThreshold)
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