package pl.edu.agh.torch.continuous.algorithm

import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.xinuk.algorithm.GridCreator
import pl.edu.agh.xinuk.model.{Grid, NonPlanarConnections}

final class TorchContinuousGridCreator(implicit config: TorchContinuousConfig) extends GridCreator {
  
  override def initialGrid: (Grid, NonPlanarConnections) = {
    val grid = Grid.empty()
    (grid, NonPlanarConnections(Set()))
  }
}

object TorchContinuousGridCreator {
  def apply(implicit config: TorchContinuousConfig): TorchContinuousGridCreator = new TorchContinuousGridCreator
}
