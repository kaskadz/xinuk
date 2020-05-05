package pl.edu.agh.torch.continuous.model.parallel

import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.torch.continuous.model.{EscapeCell, FireCell, HumanCell}
import pl.edu.agh.torch.continuous.simulation.TorchContinuousMetrics
import pl.edu.agh.xinuk.model.{Cell, GridPart, Obstacle}
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object TorchContinuousConflictResolver extends ConflictResolver[TorchContinuousConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: GridPart)(implicit config: TorchContinuousConfig): (GridPart, TorchContinuousMetrics) = {
    (current, incoming) match {
      case (EscapeCell(currentSmell), HumanCell(_, _)) =>
        (EscapeCell(currentSmell), TorchContinuousMetrics(0, 0, 0, 0, 1))
      case (EscapeCell(_), FireCell(incomingCell)) =>
        (FireCell(incomingCell), TorchContinuousMetrics.empty())
      case (FireCell(currentSmell), FireCell(incomingSmell)) =>
        (FireCell(currentSmell + incomingSmell), TorchContinuousMetrics.empty())
      case (FireCell(currentSmell), HumanCell(incomingSmell, _)) =>
        (FireCell(currentSmell + incomingSmell), TorchContinuousMetrics(0, 0, 0, 1, 0))
      case (HumanCell(currentSmell, _), FireCell(incomingSmell)) =>
        (FireCell(currentSmell + incomingSmell), TorchContinuousMetrics(0, 0, 0, 1, 0))
      case (HumanCell(currentSmell, currentCrowd), HumanCell(incomingSmell, incomingCrowd)) =>
        (HumanCell(currentSmell + incomingSmell, currentCrowd ++ incomingCrowd), TorchContinuousMetrics.empty())
      case (Obstacle, _) => (Obstacle, TorchContinuousMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
