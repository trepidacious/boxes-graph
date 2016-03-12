package org.rebeam.boxes.graph.fx.demo

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage

import org.rebeam.boxes.graph.fx._
import org.rebeam.boxes.graph._

import scalafx.scene.Scene
import scalafx.scene.Group
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

object GraphCanvasFXDemo extends JFXApp {

	val group = new Group()

	val spaces = GraphSpacesLinear(
		dataArea = Area(Vec2(0, 0), Vec2(1, 1)), 
		pixelArea = Area(Vec2(10, 10), Vec2(180, 180)), 
		componentArea = Area(Vec2(0, 0), Vec2(200, 200))
	)
	val graphCanvas = GraphCanvasFX(spaces)

	// val g = graphCanvas.g
	// g.setFill(Color.Blue)
	// g.fillRect(75,75,100,100)

	graphCanvas.color = java.awt.Color.RED
	graphCanvas.fillRect(Area(Vec2(30, 10), Vec2(50, 50)))
	graphCanvas.line(Vec2(0, 0), Vec2(100, 100))
 
	group.getChildren().add(graphCanvas.canvas)

  stage = new PrimaryStage {
    title = "GraphCanvasFX Test"
    scene = new Scene {
      fill = Color.LightGrey
      content = group
    }
  }

}