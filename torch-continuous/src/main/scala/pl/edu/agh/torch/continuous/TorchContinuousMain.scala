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
      case humanCell@HumanCell(_, _) => humanCellToColor(humanCell)
      case FireCell(_) => Color.ORANGE
      case EscapeCell(_) => new Color(139, 69, 19)
      case cell: SmellingCell => cellToColorRegions(cell)
    }
  }

  private def humanCellToColor(cell: HumanCell): Color = {
    if (cell.crowd.nonEmpty) {
      Color.WHITE
    } else {
      cellToColorRegions(cell)
    }
  }

  private def cellToColorRegions(cell: SmellingCell): Color = {
    val smellValue = cell.smell.values.map(_.value).max.toFloat
    val brightness = Math.pow(Math.abs(smellValue), 0.1).toFloat
    if (smellValue < -0.001) {
      val hue = 1f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.001) {
      val hue = 0.65f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.1) {
      val hue = 0.28f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else {
      val hue = 0.11f
      val saturation = 0.69f
      Color.getHSBColor(hue, saturation, brightness)
    }
  }
}
