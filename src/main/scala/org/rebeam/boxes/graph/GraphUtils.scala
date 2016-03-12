package org.rebeam.boxes.graph

import org.rebeam.boxes.swing.SwingView

import org.rebeam.boxes.core._
import org.rebeam.boxes.core.util._
import BoxTypes._
import BoxUtils._
import BoxScriptImports._

import java.awt.Color

object GraphUtils {
  val shadowColor = new Color(220, 220, 220)
  val shadowOffset = Vec2(1, 1)
  val barShadowColor = new Color(215, 215, 215)
  val barOutlineColor = SwingView.dividingColor.darker()
  val barOutlineUnselectedColor = SwingView.dividingColor
  val barShadowOffset = Vec2(3, 3)
  val unselectedColor = new Color(230, 230, 230)
  
  /**
   * Scale a Color by the same factor across red, green and blue,
   * then clip to 0-255 and return as a new Color
   * @param c     The input color
   * @param factor  The factor
   * @return      The output scaled color
   */
   def scaleColor(c: Color, factor: Double) = new Color( 
     clip((c.getRed() * factor).asInstanceOf[Int], 0, 255), 
     clip((c.getGreen() * factor).asInstanceOf[Int], 0, 255),
     clip((c.getBlue() * factor).asInstanceOf[Int], 0, 255)
   )
    
  /**
   * Scale a Color alpha value by a factor,
   * then clip to 0-255 and return as a new Color
   * @param c     The input color
   * @param factor  The factor
   * @return      The output scaled color
   */
   def transparentColor(c: Color, factor: Double) = new Color( 
     c.getRed(), 
     c.getGreen(),
     c.getBlue(),
     clip((c.getAlpha() * factor).asInstanceOf[Int], 0, 255)
   )
  
  /**
   * Fade a Color to white by the same factor across red, green and blue,
   * then clip to 0-255 and return as a new Color
   * @param c     The input color
   * @param factor  The factor
   * @return      The output faded color
   */
   def fadeColorToWhite(c: Color, factor: Double) = new Color( 
     clip((lerp(c.getRed(), 255, factor).asInstanceOf[Int]), 0, 255), 
     clip((lerp(c.getGreen(), 255, factor).asInstanceOf[Int]), 0, 255),
     clip((lerp(c.getBlue(), 255, factor).asInstanceOf[Int]), 0, 255)
   )
    
  /**
   * Blend from one Color to another
   * then clip to 0-255 and return as a new Color
   * @param first     The first input color
   * @param second      The second input color
   * @param factor  The factor - 0 gives first color, 1 gives second, values in between
   *          interpolate, values outside 0-1 extrapolate (but are clipped)
   * @return      The output scaled color
   */
   def blendColors(c1: Color, c2: Color, factor: Double) = new Color( 
     clip((lerp(c1.getRed(), c2.getRed(), factor).asInstanceOf[Int]), 0, 255), 
     clip((lerp(c1.getGreen(), c2.getGreen(), factor).asInstanceOf[Int]), 0, 255),
     clip((lerp(c1.getBlue(), c2.getBlue(), factor).asInstanceOf[Int]), 0, 255)
   )
 
    
  /**
   * Linearly interpolate/extrapolate from one double to another, by a certain
   * scale
   * @param from    The value returned when by == 0 
   * @param to    The value returned when by == 1
   * @param by    The interpolation/extrapolation position
   * @return      The lerped value
   */
  def lerp(from: Double, to: Double, by: Double) =  (from * (1-by)) + to * by
  
  /**
   * Return a double value clipped to lie from min to max, inclusive
   * @param value   The value
   * @param min   The minimum (inclusive)
   * @param max   The maximum (inclusive)
   * @return      The clipped value
   */
  def clip(value: Double, min: Double, max: Double) = if (value < min) min else if (value > max) max else value
  
  /**
   * Return an int value clipped to lie from min to max, inclusive
   * @param value   The value
   * @param min   The minimum (inclusive)
   * @param max   The maximum (inclusive)
   * @return      The clipped value
   */
  def clip(value: Int, min: Int, max: Int) = if (value < min) min else if (value > max) max else value
  
  def buildSpaces(graph: BoxR[Graph], componentSize: Vec2): BoxScript[GraphSpaces] = {
    for {
      g <- graph
      area <- g.dataArea      
      borders <- g.borders
    } yield {
      val w = componentSize.x.asInstanceOf[Int]
      val h = componentSize.y.asInstanceOf[Int]
  
      val l = borders.left.asInstanceOf[Int]
      val r = borders.right.asInstanceOf[Int]
      val t = borders.top.asInstanceOf[Int]
      val b = borders.bottom.asInstanceOf[Int]
      val dw = w - l - r
      val dh = h - t - b
  
      GraphSpacesLinear(
        dataArea = area, 
      // GraphSpacesLog(
      //   desiredDataArea = area, 
        pixelArea = Area(Vec2(l, t+dh), Vec2(dw, -dh)),     //The pixel area to which we map data area when drawing data is within the graph borders  
        componentArea = Area(Vec2.zero, componentSize)               //The component area is the whole of the JPanel component
      )
    }
  }

}