package org.rebeam.boxes.graph.swing.demo

import org.rebeam.boxes.swing._
import org.rebeam.boxes.graph._
import org.rebeam.boxes.graph.swing._
import org.rebeam.boxes.core._


import java.awt.Color
import javax.swing.JFrame
import java.awt.Dimension

import BoxTypes._
import BoxScriptImports._
import BoxUtils._

object GraphSwingDemoApp extends App {

  SwingView.later {
    SwingView.nimbus

    val series = new Series("Key", List(Vec2(0,0), Vec2(1,1)))
    
    val graph = Charts.withSeries(
        series = just(List(series)),
        manualBounds = atomic { create(None: Option[Area]) },
        selection = atomic { create(Set.empty[String]) },
        zoomEnabled = just(true)
        // grabEnabled = BoxNow(true)
    )
    
    val view = GraphSwingView(just(graph))
    
    val frame = new JFrame("Graph Swing Demo")

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    view.component.setMinimumSize(new Dimension(400, 400))
    view.component.setPreferredSize(new Dimension(400, 400))
    view.component.setSize(new Dimension(400, 400))
    frame.add(view.component);
    frame.pack()
    frame.setVisible(true)
  }

}