package org.rebeam.boxes.graph

trait GraphSpaces {

  def toPixel(dataPos: Vec2): Vec2
  
  def toData(pixelPos: Vec2): Vec2

  def toPixel(area: Area): Area
  
  def toData(area: Area): Area

  def dataArea: Area

  def pixelArea: Area

  def componentArea: Area

}

case class GraphSpacesLinear(val dataArea: Area, val pixelArea: Area, val componentArea: Area) extends GraphSpaces {
  
  def toPixel(dataPos: Vec2): Vec2 = pixelArea.fromUnit(dataArea.toUnit(dataPos))
  
  def toData(pixelPos: Vec2): Vec2 = dataArea.fromUnit(pixelArea.toUnit(pixelPos))

  def toPixel(area: Area): Area = {
    val o = toPixel(area.origin)
    val s = area.size / dataArea.size * pixelArea.size
    Area(o, s)
  }
  
  def toData(area: Area): Area = {
    val o = toData(area.origin)
    val s = area.size / pixelArea.size * dataArea.size
    Area(o, s)
  }
}
