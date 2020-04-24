package pl.edu.agh.torch.continuous.algorithm

import com.avsystem.commons
import com.avsystem.commons.misc.Opt
import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.torch.continuous.model.{EscapeAccessible, EscapeCell, FireAccessible, FireCell, HumanAccessible, HumanCell}
import pl.edu.agh.torch.continuous.simulation.TorchContinuousMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, EnhancedGrid, Grid, GridPart, Obstacle, Signal}
import pl.edu.agh.xinuk.simulation.Metrics

import scala.util.Random

final class TorchContinuousMovesController(implicit config: TorchContinuousConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  def calculatePossibleDestinations(cell: HumanCell, x: Int, y: Int, grid: EnhancedGrid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map {
        case (i, j) => cell.smell(i)(j)
      }
      .zipWithIndex
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map {
        case (_, idx) =>
          val (i, j) = neighbourCellCoordinates(idx)
          (i, j, grid.cells(i)(j))
      }
  }

  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: EnhancedGrid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map {
        case (i, j, current) => (i, j, current, newGrid.cells(i)(j))
      }
      .collectFirstOpt {
        case (i, j, currentCell@HumanAccessible(_), HumanAccessible(_)) =>
          (i, j, currentCell)
      }
  }

  override def makeMoves(iteration: Long, grid: EnhancedGrid): (EnhancedGrid, Metrics) = {
    val newGrid = grid.emptyCopy()

    var humanCount = 0L
    var fireCount = 0L
    var escapesCount = 0L
    var peopleDeaths = 0L
    var peopleEscaped = 0L

    def isEmptyIn(grid: EnhancedGrid)(i: Int, j: Int): Boolean = {
      grid.getCellAt(i, j).cell match {
        case EmptyCell(_) | BufferCell(EmptyCell(_)) => true
        case _ => false
      }
    }

    def reproduce(x: Int, y: Int)(creator: PartialFunction[GridPart, GridPart]): Unit = {
      val availableCells =
        Grid.neighbourCellCoordinates(x, y).flatMap {
          case (i, j) =>
            grid.cells(i)(j).opt
              .filter(_ => creator.isDefinedAt(newGrid.cells(i)(j))) //use the same availability criteria on new grid
              .collect(creator)
              .map((i, j, _))
        }
      if (availableCells.nonEmpty) {
        val (newFireX, newFireY, newCell) = availableCells(random.nextInt(availableCells.size))
        newGrid.cells(newFireX)(newFireY) = newCell
        grid.cells(newFireX)(newFireY) match {
          case HumanCell(_, _, _) =>
            peopleDeaths += 1
          case _ =>
        }
      }
    }

    def makeMove(x: Int, y: Int): Unit = {
      grid.getCellAt(x, y).cell match {
        case Obstacle =>
          newGrid.setCellAt(x, y) = Obstacle
        case cell@(EmptyCell(_) | BufferCell(_)) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.setCellAt(x, y) = cell
          }
        case EscapeCell(_) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.setCellAt(x, y) = EscapeAccessible.unapply(EmptyCell.Instance).withEscape()
          }
        case cell: FireCell =>
          if (iteration % config.fireSpeadingFrequency == 0) {
            reproduce(x, y) {
              case FireAccessible(accessible) => accessible.withFire()
            }
          }
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.setCellAt(x, y) = cell.copy()
          }
        case cell: HumanCell =>
          newGrid.getCellAt(x, y).cell match {
            case FireCell(_) =>
            case _ => if (iteration % cell.speed == 0) {
              moveHuman(cell, x, y)
            } else {
              stayInPlace(cell, x, y)
            }
          }
      }
    }

    def stayInPlace(cell: HumanCell, x: Int, y: Int): Unit = {
      newGrid.setCellAt(x, y) = cell.copy(cell.smell, cell.crowd, cell.speed)
    }

    def moveHuman(cell: HumanCell, x: Int, y: Int): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)
      if (cell.crowd.isEmpty) {
        destination match {
          case Opt((i, j, HumanAccessible(destination))) =>
            newGrid.setCellAt(i, j) = destination.withHuman(cell.crowd, cell.speed)
            newGrid.getCellAt(i, j).cell match {
              case EscapeCell(_) => peopleEscaped += 1
              case _ =>
            }
          case Opt((i, j, inaccessibleDestination)) =>
            throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
          case Opt.Empty =>
            newGrid.setCellAt(x, y) = cell.copy(cell.smell, cell.crowd, cell.speed)
        }
      } else {
        destination match {
          case Opt((i, j, HumanAccessible(destination))) =>
            newGrid.setCellAt(i, j) = destination.withHuman(cell.crowd.head.crowd, cell.crowd.head.speed)
            newGrid.setCellAt(x, y) = cell.copy(cell.smellWithout(cell.crowd.head.smell), cell.crowd.drop(1), cell.speed)
          case Opt((i, j, inaccessibleDestination)) =>
            throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
          case Opt.Empty =>
            newGrid.setCellAt(x, y) = cell.copy(cell.smell, cell.crowd, cell.speed)
        }
      }

    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      grid.getCellAt(x, y).cell match {
        case HumanCell(_, crowd, _) =>
          humanCount += 1 + crowd.size
        case FireCell(_) =>
          fireCount += 1
        case EscapeCell(_) =>
          escapesCount += 1
        case _ =>
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } makeMove(x, y)

    val metrics = TorchContinuousMetrics(humanCount, fireCount, escapesCount, peopleDeaths, peopleEscaped)
    (newGrid, metrics)
  }
}

object TorchContinuousMovesController {
  def apply(implicit config: TorchContinuousConfig): TorchContinuousMovesController = new TorchContinuousMovesController
}