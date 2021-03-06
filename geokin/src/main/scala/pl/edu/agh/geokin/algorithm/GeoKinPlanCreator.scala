package pl.edu.agh.geokin.algorithm

import pl.edu.agh.geokin.config.GeoKinConfig
import pl.edu.agh.geokin.model.{Runner, RunnerOccupied}
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model.{CellContents, CellId, CellState, Direction, SignalMap}

import scala.util.Random

final case class GeoKinPlanCreator() extends PlanCreator[GeoKinConfig] {

  override def createPlans(iteration: Long,
                           cellId: CellId,
                           cellState: CellState,
                           neighbourContents: Map[Direction, CellContents])
                          (implicit config: GeoKinConfig): (Plans, GeoKinMetrics) = {
    cellState.contents match {
      case ro@RunnerOccupied(_) =>
        (moveRunners(ro, iteration, cellState.signalMap, neighbourContents), GeoKinMetrics.empty)
      case _ => (Plans.empty, GeoKinMetrics.empty)
    }
  }

  private def moveRunners(cell: RunnerOccupied,
                          iteration: Long,
                          signalMap: SignalMap,
                          neighbourContents: Map[Direction, CellContents]): Plans = {
    val runners = Random.shuffle(cell.runners)

    val plans: Map[Option[Direction], Seq[Plan]] = runners
      .map(moveRunner(_, iteration, signalMap, neighbourContents))
      .groupMap { case (maybeDirection, _) => maybeDirection } { case (_, plan) => plan }

    val localPlans = plans.getOrElse(Option.empty, Seq.empty)
    val outwardPlans = plans
      .filterNot { case (maybeDirection, _) => maybeDirection.isEmpty }
      .map { case (maybeDirection, value) => (maybeDirection.get, value) }

    new Plans(outwardPlans, localPlans)
  }

  def moveRunner(runner: Runner,
                 iteration: Long,
                 signalMap: SignalMap,
                 neighbourContents: Map[Direction, CellContents]): (Option[Direction], Plan) = {
    ???
  }
}
