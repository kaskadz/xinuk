package pl.edu.agh.torch.continuous.model.parallel

import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.torch.continuous.model.{EscapeCell, FireCell, HumanCell}
import pl.edu.agh.torch.continuous.simulation.TorchContinuousMetrics
import pl.edu.agh.xinuk.model.{Cell, EmptyCell, GridPart, Obstacle, SmellingCell}
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object TorchContinuousConflictResolver extends ConflictResolver[TorchContinuousConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: GridPart)(implicit config: TorchContinuousConfig): (GridPart, TorchContinuousMetrics) = {
    (current, incoming) match {
      case (EmptyCell(currentSmell), incomingCell: SmellingCell) =>
        (incomingCell.withSmell(incomingCell.smell + currentSmell), TorchContinuousMetrics.empty())
      case (currentCell: SmellingCell, EmptyCell(incomingSmell)) =>
        (currentCell.withSmell(currentCell.smell + incomingSmell), TorchContinuousMetrics.empty())
      case (EscapeCell(currentSmell), HumanCell(_, _, _)) =>
        (EscapeCell(currentSmell), TorchContinuousMetrics(0, 0, 0, 0, 1))
      case (EscapeCell(_), FireCell(incomingCell)) =>
        (FireCell(incomingCell), TorchContinuousMetrics.empty())
      case (FireCell(currentSmell), FireCell(incomingSmell)) =>
        (FireCell(currentSmell + incomingSmell), TorchContinuousMetrics.empty())
      case (FireCell(currentSmell), HumanCell(incomingSmell, _, _)) =>
        (FireCell(currentSmell + incomingSmell), TorchContinuousMetrics(0, 0, 0, 1, 0))
      case (HumanCell(currentSmell, _, _), FireCell(incomingSmell)) =>
        (FireCell(currentSmell + incomingSmell), TorchContinuousMetrics(0, 0, 0, 1, 0))
      case (HumanCell(currentSmell, currentCrowd, currentSpeed), another@HumanCell(incomingSmell, incomingCrowd, _)) =>
        (HumanCell(currentSmell + incomingSmell, currentCrowd ++ incomingCrowd ++ List(another), currentSpeed), TorchContinuousMetrics.empty())
      case (Obstacle, _) => (Obstacle, TorchContinuousMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
