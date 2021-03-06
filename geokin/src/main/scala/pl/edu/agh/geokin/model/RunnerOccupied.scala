package pl.edu.agh.geokin.model

import pl.edu.agh.geokin.config.GeoKinConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, Signal}

final case class RunnerOccupied(runners: List[Runner]) extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal =
    config.asInstanceOf[GeoKinConfig].singleRunnerInitialSignal * runners.size
}
