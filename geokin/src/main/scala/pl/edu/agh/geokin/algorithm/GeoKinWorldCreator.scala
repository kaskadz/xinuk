package pl.edu.agh.geokin.algorithm

import pl.edu.agh.geokin.config.GeoKinConfig
import pl.edu.agh.xinuk.algorithm.WorldCreator
import pl.edu.agh.xinuk.model.WorldBuilder

object GeoKinWorldCreator extends WorldCreator[GeoKinConfig] {
  override def prepareWorld()(implicit config: GeoKinConfig): WorldBuilder = {
    ???
  }
}
