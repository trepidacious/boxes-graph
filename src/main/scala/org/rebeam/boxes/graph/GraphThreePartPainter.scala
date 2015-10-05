package org.rebeam.boxes.graph

import java.awt.Image
import org.rebeam.boxes.swing.ImageThirds

//Draw a horizontal component made up of a left, middle and right portion. Portions are
//taken from the thirds of an image, and middle is stretched horizontally to fit.
class GraphThreePartPainter(image: Image) {
  val thirds = ImageThirds.horizontalImageThirds(image)

  def paint(canvas: GraphCanvas, p: Vec2, s: Vec2) {
    val middle = s.x - thirds.pieceWidth * 2
    canvas.image(thirds.parts._1, p)
    canvas.image(thirds.parts._3, p + Vec2(s.x - thirds.pieceWidth))
    if (middle > 0) {
      canvas.image(thirds.parts._2, p + Vec2(thirds.pieceWidth, 0), Vec2(middle, thirds.pieceHeight))
    }
  }
}

class GraphThreePartPainterVertical(image:Image) {
  val thirds = ImageThirds.verticalImageThirds(image)

  def paint(canvas: GraphCanvas, p: Vec2, s: Vec2) {
    val middle = s.y - thirds.pieceHeight * 2
    canvas.image(thirds.parts._1, p)
    canvas.image(thirds.parts._3, p + Vec2(0, s.y - thirds.pieceHeight))
    if (middle > 0) {
      canvas.image(thirds.parts._2, p + Vec2(0, thirds.pieceHeight), Vec2(thirds.pieceWidth, middle))
    }
  }
}