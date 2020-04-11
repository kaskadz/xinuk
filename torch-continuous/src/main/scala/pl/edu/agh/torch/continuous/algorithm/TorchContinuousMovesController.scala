package pl.edu.agh.torch.continuous.algorithm

import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.torch.continuous.simulation.TorchContinuousMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.EnhancedGrid
import pl.edu.agh.xinuk.simulation.Metrics

final class TorchContinuousMovesController(implicit config: TorchContinuousConfig) extends MovesController {

  override def makeMoves(iteration: Long, grid: EnhancedGrid): (EnhancedGrid, Metrics) = {
    val newGrid = grid.emptyCopy()
    (newGrid, TorchContinuousMetrics.empty())
  }
}

object TorchContinuousMovesController {
  def apply(implicit config: TorchContinuousConfig): TorchContinuousMovesController = new TorchContinuousMovesController
}