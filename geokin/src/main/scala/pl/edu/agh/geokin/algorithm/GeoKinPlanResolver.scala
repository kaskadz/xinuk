package pl.edu.agh.geokin.algorithm

import pl.edu.agh.geokin.config.GeoKinConfig
import pl.edu.agh.xinuk.algorithm.{Metrics, PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.{CellContents, Empty}
import pl.edu.agh.geokin.model.{Destination, RunnerOccupied, Threat}

final case class GeoKinPlanResolver() extends PlanResolver[GeoKinConfig] {
  override def isUpdateValid(contents: CellContents, update: StateUpdate)(implicit config: GeoKinConfig): Boolean =
    (contents, update.value) match {
      case (Destination, Threat) => true // Threat spreads
      case (Destination, RunnerOccupied(_)) => true // Runner finishes
      case (RunnerOccupied(_), RunnerOccupied(_)) => true // Runners crowd up
      case (RunnerOccupied(_), Empty) => true // Cell becomes empty
      case (RunnerOccupied(_), Threat) => true // Threat spreads
      case (Empty, RunnerOccupied(_)) => true // Runner enters cell
      case (Empty, Threat) => true // Threat spreads
      case (Threat, RunnerOccupied(_)) => true // Runners encounter threat
      case _ => false
    }

  override def applyUpdate(contents: CellContents, update: StateUpdate)
                          (implicit config: GeoKinConfig): (CellContents, Metrics) = {
    (contents, update.value) match {
      case (_, Empty) =>
        (Empty, GeoKinMetrics.empty)
      case (RunnerOccupied(runners), Threat) =>
        (Threat, GeoKinMetrics.runnersFailed(runners.size))
      case (RunnerOccupied(runners1), RunnerOccupied(runners2)) =>
        (RunnerOccupied(runners1 ++ runners2), GeoKinMetrics.empty)
      case (Threat, RunnerOccupied(runners)) =>
        (Threat, GeoKinMetrics.runnersFailed(runners.size))
      case (Empty, ro@RunnerOccupied(_)) =>
        (ro, GeoKinMetrics.empty)
      case (Destination, RunnerOccupied(runners)) =>
        (Destination, GeoKinMetrics.runnersDone(runners.size))
      case (Destination, Threat) =>
        (Threat, GeoKinMetrics.empty)
      case _ => throw new IllegalArgumentException(s"Illegal update applied: state = $contents, update = $update")
    }
  }
}
