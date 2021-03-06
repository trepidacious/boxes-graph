package org.rebeam.boxes.graph

import Axis._
import GraphMouseEventType._
import java.awt.Color
import java.text.DecimalFormat
import org.rebeam.boxes.core._
import org.rebeam.boxes.core.BoxScriptImports._
import org.rebeam.boxes.core.BoxTypes._
import org.rebeam.boxes.core.BoxUtils._
import org.rebeam.boxes.swing.SwingView
import scalaz._
import scalaz.Scalaz._

object Charts {
  
  val defaultBorders = Borders(16, 74, 53, 16)

  val defaultZoomBoxFillColor = new Color(0, 0, 200, 50)
  val defaultZoomBoxOutlineColor = new Color(100, 100, 200)

  val defaultSelectBoxFillColor = new Color(0, 200, 0, 50)
  val defaultSelectBoxOutlineColor = new Color(100, 200, 100)

  def withSeries[K](
      series: BoxR[List[Series[K]]],
      xName: BoxR[String] = just("x"),
      yName: BoxR[String] = just("y"),
      borders: BoxR[Borders] = just(defaultBorders),
      zoomEnabled: BoxR[Boolean] = just(false),
      manualBounds: BoxM[Option[Area]],
      xAxis: BoxR[GraphZoomerAxis] = just(GraphDefaults.axis),
      yAxis: BoxR[GraphZoomerAxis] = just(GraphDefaults.axis),
      selectEnabled: BoxR[Boolean] = just(false),
      clickSelectEnabled: BoxR[Boolean] = just(true),
      selection: BoxM[Set[K]],
      grabEnabled: BoxR[Boolean] = just(false),
      seriesTooltipsEnabled: BoxR[Boolean] = just(true),
      seriesTooltipsPrinter: TooltipPrinter[K] = new StringTooltipPrinter[K](),
      axisTooltipsEnabled: BoxR[Boolean] = just(true),
      extraMainLayers: List[GraphLayer] = Nil,
      extraOverLayers: List[GraphLayer] = Nil,
      highQuality: BoxR[Boolean] = just(true),
      border: Color = SwingView.background,
      background: Color = Color.white,
      seriesShadow: BoxR[Boolean] = just(true)
    ) = {

    val layers = atomic{ 
      create(
        extraMainLayers ::: List(
          new GraphBG(border, background),
          // new GraphHighlight(),
          new GraphSeries(series, just(true), seriesShadow),  //This layer would always draw series shadows, but is only enabled if we want them
          new GraphAxis(Y, 50),
          new GraphAxis(X),
          // new GraphShadow(),
          new GraphSeries(series),  //This layer draws series themselves (not shadows), and is always enabled
          new GraphOutline(),
          new GraphAxisTitle(X, xName),
          new GraphAxisTitle(Y, yName)
        )
      )
    }

    val viewRegion = GraphLayer.combinedViewRegion(layers, just(extraOverLayers))

    val zoomer = new GraphZoomer(viewRegion, manualBounds(), xAxis, yAxis)

    val overlayers = atomic {
      create (
        List(SeriesTooltips.highlight(series, seriesTooltipsEnabled)) ::: extraOverLayers ::: List(
          GraphZoomBox(just(defaultZoomBoxFillColor), just(defaultZoomBoxOutlineColor), manualBounds, zoomEnabled),
          GraphSelectBox(series, just(defaultSelectBoxFillColor), just(defaultSelectBoxOutlineColor), selection, selectEnabled),
          GraphGrab(grabEnabled, manualBounds, zoomer.dataArea),
          GraphClickToSelectSeries(series, selection, clickSelectEnabled),
          AxisTooltip(X, axisTooltipsEnabled),
          AxisTooltip(Y, axisTooltipsEnabled),
          SeriesTooltips.string(series, seriesTooltipsEnabled, seriesTooltipsPrinter)
        )
      )
    }

    new GraphBasic(
      layers(),
      overlayers(),
      zoomer.dataArea,
      borders,
      highQuality
    )
  }
  
  
//   def withBarsSelectByCat[C1, C2, K](
//       data: Box[Map[(C1, C2), Bar[K]]],
//       cat1Print: (C1 => String) = (c: C1) => c.toString, 
//       cat2Print: (C2 => String) = (c: C2) => c.toString,
//       barWidth: Box[Double] = BoxNow(1.0), 
//       catPadding: Box[Double] = BoxNow(1.0), 
//       barPadding: Box[Double] = BoxNow(0.4),
//       yName: Box[String] = BoxNow("y"),
//       borders: Box[Borders] = BoxNow(Borders(16, 74, 53, 16)),
//       zoomEnabled: Box[Boolean] = BoxNow(true),
//       manualBounds: Box[Option[Area]] = BoxNow(None),
//       xAxis: Box[GraphZoomerAxis] = BoxNow(GraphDefaults.axis),
//       yAxis: Box[GraphZoomerAxis] = BoxNow(GraphDefaults.axis(0, 0.05)),
//       selectEnabled: Box[Boolean] = BoxNow(false),
//       clickSelectEnabled: Box[Boolean] = BoxNow(true),
//       selection:Box[Set[(C1, C2)]],
//       grabEnabled: Box[Boolean] = BoxNow(false),
//       barTooltipsEnabled: Box[Boolean] = BoxNow(true),
//       barTooltipsPrint: ((C1, C2, Bar[K]) => String) = BarTooltips.defaultPrint[C1, C2, K],
//       axisTooltipsEnabled: Box[Boolean] = BoxNow(true),
//       extraMainLayers:List[GraphLayer] = List[GraphLayer](),
//       extraOverLayers:List[GraphLayer] = List[GraphLayer](),
//       highQuality: Box[Boolean] = BoxNow(true)
//       )(implicit shelf: Shelf, ord1: Ordering[C1], ord2: Ordering[C2]) = {

//     val layers = BoxNow(
//       extraMainLayers ::: List(
//         new GraphBG(SwingView.background, Color.white),
//         new GraphHighlight(),
//         new GraphBars(data, barWidth, catPadding, barPadding, true),  //Shadows
//         new GraphAxis(Y, 50),
//         new GraphBarAxis(data, barWidth, catPadding, barPadding, X, cat1Print, cat2Print),
//         new GraphShadow(),
//         new GraphBars(data, barWidth, catPadding, barPadding, false), //Data
//         new GraphOutline(),
//         new GraphAxisTitle(Y, yName)
//       )
//     )

//     val dataBounds = BoxNow.calc(implicit TxnR => {
//       layers().foldLeft(None:Option[Area]){
//         (areaOption, layer) => areaOption match {
//           case None => layer.dataBounds()

//           case Some(area) => layer.dataBounds() match {
//             case None => Some(area)
//             case Some(layerArea) => Some(area.extendToContain(layerArea))
//           }
//         }
//       }
//     })

//     val zoomer = new GraphZoomer(dataBounds, manualBounds, xAxis, yAxis)

//     val overlayers = BoxNow(
// //      List(SeriesTooltips.highlight(series, seriesTooltipsEnabled)) ::: 
//       extraOverLayers ::: List(
//         GraphZoomBox(BoxNow(new Color(0, 0, 200, 50)), BoxNow(new Color(100, 100, 200)), manualBounds, zoomEnabled),
//         GraphSelectBarsByCatWithBox(data, selection, barWidth, catPadding, barPadding, selectEnabled, BoxNow(new Color(0, 200, 0, 50)), BoxNow(new Color(100, 200, 100))),
//         GraphGrab(grabEnabled, manualBounds, zoomer.dataArea),
//         GraphClickToSelectBarByCat(data, selection, barWidth, catPadding, barPadding, clickSelectEnabled),
//         AxisTooltip(Y, axisTooltipsEnabled),
//         BarTooltips.string(barTooltipsEnabled, data, barWidth, catPadding, barPadding, barTooltipsPrint)
//       )
//     )

//     new GraphBasic(
//       layers,
//       overlayers,
//       zoomer.dataArea,
//       borders,
//       highQuality
//     )
//   }
 
//   def withBarsSelectByKey[C1, C2, K](
      
//       data: Box[Map[(C1, C2), Bar[K]]],
//       cat1Print: (C1 => String) = (c: C1) => c.toString, 
//       cat2Print: (C2 => String) = (c: C2) => c.toString,
//       barWidth: Box[Double] = BoxNow(1.0), 
//       catPadding: Box[Double] = BoxNow(1.0), 
//       barPadding: Box[Double] = BoxNow(0.4),
//       yName: Box[String] = BoxNow("y"),
//       borders: Box[Borders] = BoxNow(Borders(16, 74, 53, 16)),
//       zoomEnabled: Box[Boolean] = BoxNow(true),
//       manualBounds: Box[Option[Area]] = BoxNow(None),
//       xAxis: Box[GraphZoomerAxis] = BoxNow(GraphDefaults.axis),
//       yAxis: Box[GraphZoomerAxis] = BoxNow(GraphDefaults.axis(0, 0.05)),
//       selectEnabled: Box[Boolean] = BoxNow(false),
//       clickSelectEnabled: Box[Boolean] = BoxNow(true),
//       selection: Box[Set[K]],
//       grabEnabled: Box[Boolean] = BoxNow(false),
//       barTooltipsEnabled: Box[Boolean] = BoxNow(true),
//       barTooltipsPrint: ((C1, C2, Bar[K]) => String) = BarTooltips.defaultPrint[C1, C2, K],
//       axisTooltipsEnabled: Box[Boolean] = BoxNow(true),
//       extraMainLayers:List[GraphLayer] = List[GraphLayer](),
//       extraOverLayers:List[GraphLayer] = List[GraphLayer](),
//       highQuality: Box[Boolean] = BoxNow(true)      
//       )(implicit shelf: Shelf, ord1: Ordering[C1], ord2: Ordering[C2]) = {

//     val layers = BoxNow(
//       extraMainLayers ::: List(
//         new GraphBG(SwingView.background, Color.white),
//         new GraphHighlight(),
//         new GraphBars(data, barWidth, catPadding, barPadding, true),  //Shadows
//         new GraphAxis(Y, 50),
//         new GraphBarAxis(data, barWidth, catPadding, barPadding, X, cat1Print, cat2Print),
//         new GraphShadow(),
//         new GraphBars(data, barWidth, catPadding, barPadding, false), //Data
//         new GraphOutline(),
//         new GraphAxisTitle(Y, yName)
//       )
//     )

//     val dataBounds = BoxNow.calc(implicit TxnR => {
//       layers().foldLeft(None:Option[Area]){
//         (areaOption, layer) => areaOption match {
//           case None => layer.dataBounds()

//           case Some(area) => layer.dataBounds() match {
//             case None => Some(area)
//             case Some(layerArea) => Some(area.extendToContain(layerArea))
//           }
//         }
//       }
//     })

//     val zoomer = new GraphZoomer(dataBounds, manualBounds, xAxis, yAxis)

//     val overlayers = BoxNow(
// //      List(SeriesTooltips.highlight(series, seriesTooltipsEnabled)) ::: 
//       extraOverLayers ::: List(
//         GraphZoomBox(BoxNow(new Color(0, 0, 200, 50)), BoxNow(new Color(100, 100, 200)), manualBounds, zoomEnabled),
//         GraphSelectBarsByKeyWithBox(data, selection, barWidth, catPadding, barPadding, selectEnabled, BoxNow(new Color(0, 200, 0, 50)), BoxNow(new Color(100, 200, 100))),
//         GraphGrab(grabEnabled, manualBounds, zoomer.dataArea),
//         GraphClickToSelectBarByKey(data, selection, barWidth, catPadding, barPadding, clickSelectEnabled),
//         AxisTooltip(Y, axisTooltipsEnabled),
//         BarTooltips.string(barTooltipsEnabled, data, barWidth, catPadding, barPadding, barTooltipsPrint)
//       )
//     )

//     new GraphBasic(
//       layers,
//       overlayers,
//       zoomer.dataArea,
//       borders,
//       highQuality
//     )
//   }
  
}