package pl.edu.agh.geokin.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Signal, WorldType}

final case class GeoKinConfig(worldType: WorldType,
                              worldWidth: Int,
                              worldHeight: Int,
                              iterationsNumber: Long,

                              signalSuppressionFactor: Double,
                              signalAttenuationFactor: Double,
                              signalSpeedRatio: Int,

                              workersRoot: Int,
                              isSupervisor: Boolean,
                              shardingMod: Int,

                              guiCellSize: Int,
                              guiType: GuiType,

                              personMaxSpeed: Double,
                              threatSpreadingFrequency: Int,

                              singleRunnerInitialSignal: Signal,
                              threatInitialSignal: Signal,
                              destinationInitialSignal: Signal
                             ) extends XinukConfig
