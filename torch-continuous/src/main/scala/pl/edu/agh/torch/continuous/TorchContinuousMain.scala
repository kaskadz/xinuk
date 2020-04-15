package pl.edu.agh.torch.continuous

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.torch.continuous.algorithm.{TorchContinuousGridCreator, TorchContinuousMovesController}
import pl.edu.agh.torch.continuous.model.{EscapeCell, FireCell, HumanCell}
import pl.edu.agh.torch.continuous.model.parallel.TorchContinuousConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Obstacle, SmellingCell}

object TorchContinuousMain extends LazyLogging {
  private val configPrefix = "torch-continuous"
  private val metricHeaders = Vector(
    "peopleCount",
    "fireCount",
    "escapeCount",
    "peopleDeaths",
    "peopleEscapes"
  )

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      metricHeaders,
      TorchContinuousConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    )(
      TorchContinuousGridCreator.apply(_),
      TorchContinuousMovesController.apply(_),
      {
        case Obstacle => Color.BLUE
        case cell: SmellingCell => cellToColor(cell)
      }).start()
  }

  private def cellToColor(cell: SmellingCell): Color = {
    cell match {
      case HumanCell(_, _, _) => Color.BLUE
      case FireCell(_) => Color.ORANGE
      case EscapeCell(_) => new Color(139, 69, 19)
      case _ => Color.WHITE
    }
  }
}
