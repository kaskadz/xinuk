package pl.edu.agh.torch.continuous.model.parallel

import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.torch.continuous.simulation.TorchContinuousMetrics
import pl.edu.agh.xinuk.model.{Cell, EmptyCell, GridPart}
import pl.edu.agh.xinuk.model.parallel.ConflictResolver
import pl.edu.agh.xinuk.simulation.Metrics

object TorchContinuousConflictResolver extends ConflictResolver[TorchContinuousConfig] {
  override def resolveConflict(current: GridPart, incoming: GridPart)(implicit config: TorchContinuousConfig): (GridPart, Metrics) = {
    (EmptyCell(Cell.emptySignal), TorchContinuousMetrics.empty())
  }
}
