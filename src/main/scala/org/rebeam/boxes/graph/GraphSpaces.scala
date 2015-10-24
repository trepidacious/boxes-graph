package org.rebeam.boxes.graph

trait GraphSpaces {

  def toPixel(dataPos: Vec2): Vec2
  
  def toData(pixelPos: Vec2): Vec2

  def dataArea: Area

  def pixelArea: Area

  def componentArea: Area

  def toPixel(area: Area): Area = {
    val o = toPixel(area.origin)
    val e = toPixel(area.origin + area.size)
    Area(o, e - o)
  }
  
  def toData(area: Area): Area = {
    val o = toData(area.origin)
    val e = toData(area.origin + area.size)
    Area(o, e - o)
  }

}

case class GraphSpacesLinear(val dataArea: Area, val pixelArea: Area, val componentArea: Area) extends GraphSpaces {
  
  def toPixel(dataPos: Vec2): Vec2 = pixelArea.fromUnit(dataArea.toUnit(dataPos))
  
  def toData(pixelPos: Vec2): Vec2 = dataArea.fromUnit(pixelArea.toUnit(pixelPos))

}

case class GraphSpacesLog(val desiredDataArea: Area, val pixelArea: Area, val componentArea: Area, val minX: Option[Double] = None, val minY: Option[Double] = Some(0.0001)) extends GraphSpaces {
  
  val dataArea = {
    val a = desiredDataArea.normalise
    val a2 = minX match {
      case Some(m) => if (a.origin.x < m) Area(Vec2(m, a.origin.y), a.size) else a
      case None => a
    }
    minY match {
      case Some(m) => if (a2.origin.y < m) Area(Vec2(a2.origin.x, m), a2.size) else a2
      case None => a2
    }
  }

  def toPixel(dataPos: Vec2): Vec2 = {

    //Convert with linear scale - take log of data point and area, and
    //use this to find position in unit scale
    val dataLog = dataPos.log10
    val dataLogArea = dataArea.log10
    val dataLogUnit = dataLogArea.toUnit(dataLog)

    //Convert unit position into pixel space
    val pixelLog = pixelArea.fromUnit(dataLogUnit)

    //Now a plain linear conversion for any axis that needs this
    val pixelLinear = pixelArea.fromUnit(dataArea.toUnit(dataPos))

    (minX, minY) match {
      case (Some(_), Some(_)) => pixelLog
      case (Some(_), None) => Vec2(pixelLog.x, pixelLinear.y)
      case (None, Some(_)) => Vec2(pixelLinear.x, pixelLog.y)
      case (None, None) => pixelLinear
    }
  }
  
  def toData(pixelPos: Vec2): Vec2 = {

    //Find unit position in pixel space
    val pixelUnit = pixelArea.toUnit(pixelPos)

    //Convert to unit position in log data space, then
    //convert back into data linear space using pow10
    val dataLogArea = dataArea.log10
    val pixelToDataLog = dataLogArea.fromUnit(pixelUnit)
    val dataLog = pixelToDataLog.pow10

    //Now a plain linear conversion for any axis that needs this
    val dataLinear = dataArea.fromUnit(pixelUnit)

    (minX, minY) match {
      case (Some(_), Some(_)) => dataLog
      case (Some(_), None) => Vec2(dataLog.x, dataLinear.y)
      case (None, Some(_)) => Vec2(dataLinear.x, dataLog.y)
      case (None, None) => dataLinear
    }
  }

}