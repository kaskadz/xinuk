package pl.edu.agh.geokin.model

import pl.edu.agh.geokin.config.GeoKinConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, Signal}

case object Threat extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal =
    config.asInstanceOf[GeoKinConfig].threatInitialSignal
}
