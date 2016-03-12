package org.rebeam.boxes.graph.fx.demo

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage

import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.Group
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.layout.AnchorPane
import scalafx.scene.layout.BorderPane
import scalafx.geometry.Insets

import scalafx.scene.layout.Background
import scalafx.scene.layout.BackgroundFill
import scalafx.scene.layout.CornerRadii

import org.rebeam.boxes.graph.fx._
import org.rebeam.boxes.graph._
import org.rebeam.boxes.core._

import BoxTypes._
import BoxScriptImports._
import BoxUtils._

import Axis._

object GraphFXViewDemo extends JFXApp {

	val series1 = new Series("Key 1", List(Vec2(0.1,0.1), Vec2(1,1)), java.awt.Color.blue, 2)
	val series2 = new Series("Key 2", List(Vec2(0.1,1), Vec2(1,0.1)), java.awt.Color.red, 2)
	
	val selection = atomic { create(Set.empty[String]) }

	val x = atomic { create(0.5d) }
	val xThreshold = GraphThreshold(just(X), x, just(java.awt.Color.blue), just("X Threshold"), just(true))

	val y = atomic { create(0.5d) }
	val yThreshold = GraphThreshold(just(Y), y, just(java.awt.Color.red), just("Y Threshold"), just(true))

	val series = just(List(series1, series2))

	val seriesBySelection = ColorSeriesBySelection(series, selection)

	val graph = Charts.withSeries(
			series = seriesBySelection,
			manualBounds = atomic { create(None: Option[Area]) },
			selection = selection,
			zoomEnabled = just(false),
			grabEnabled = just(true),
			extraOverLayers = List(xThreshold, yThreshold)
	)

	val view = GraphFXView(just(graph))

  stage = new PrimaryStage {
    title = "GraphFXView Test"
    scene = new Scene(new javafx.scene.Scene(view.component))
  }

}