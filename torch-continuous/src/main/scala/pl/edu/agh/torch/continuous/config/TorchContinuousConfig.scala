package pl.edu.agh.torch.continuous.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.Signal

final case class TorchContinuousConfig(
                                        gridSize: Int,
                                        guiCellSize: Int,
                                        signalSuppressionFactor: Double,
                                        signalAttenuationFactor: Double,
                                        workersRoot: Int,
                                        shardingMod: Int,

                                        guiType: GuiType,
                                        isSupervisor: Boolean,
                                        signalSpeedRatio: Int,
                                        iterationsNumber: Long,

                                        humanMaxSpeed: Int,
                                        humanMinSpeed: Int,
                                        fireSpeadingFrequency: Int,
                                        spawnChance: Double,
                                        humanSpawnChance: Double,
                                        fireSpawnChance: Double,
                                        escapeSpawnChance: Double,
                                        humanInitialSignal: Signal,
                                        fireInitialSignal: Signal,
                                        escapeInitialSignal: Signal,
                                        cellSize: Int
                                      ) extends XinukConfig
