package pl.edu.agh.torch.continuous.algorithm

import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.torch.continuous.model.{EscapeAccessible, EscapeCell, FireAccessible, FireCell, Human, HumanAccessible, HumanCell}
import pl.edu.agh.torch.continuous.simulation.TorchContinuousMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.Direction.Direction
import pl.edu.agh.xinuk.model.{BufferCell, Direction, EmptyCell, EnhancedGrid, GridPart, LocalEnhancedCell, Obstacle, Signal, SmellingCell}
import pl.edu.agh.xinuk.simulation.Metrics

import scala.util.Random

final class TorchContinuousMovesController(implicit config: TorchContinuousConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  def calculatePossibleDestinations(cell: HumanCell, x: Int, y: Int, grid: EnhancedGrid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = grid.neighbourCellCoordinates(x, y)
    grid.neighbourCellDirections(x, y)
      .map(dir => cell.smell(dir))
      .zipWithIndex
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map {
        case (_, idx) =>
          val (i, j) = neighbourCellCoordinates(idx)
          (i, j, grid.getCellAt(i, j).cell)
      }
  }

  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: EnhancedGrid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map {
        case (i, j, current) => (i, j, current, newGrid.getCellAt(i, j).cell)
      }
      .collectFirstOpt {
        case (i, j, currentCell@HumanAccessible(_), HumanAccessible(_)) =>
          (i, j, currentCell)
      }
  }

  override def makeMoves(iteration: Long, grid: EnhancedGrid): (EnhancedGrid, Metrics) = {
    val newGrid = grid.emptyCopy{ HumanCell.Instance }

    var humanCount = 0L
    var fireCount = 0L
    var escapesCount = 0L
    var peopleDeaths = 0L
    var peopleEscaped = 0L

    def isEmptyIn(grid: EnhancedGrid)(i: Int, j: Int): Boolean = {
      grid.getCellAt(i, j).cell match {
        case EmptyCell(_) | BufferCell(EmptyCell(_)) => true
        case humanCell@HumanCell(_, _) => humanCell.crowd.isEmpty
        case _ => false
      }
    }

    def reproduce(x: Int, y: Int)(creator: PartialFunction[GridPart, GridPart]): Unit = {
      val availableCells =
        grid.neighbourCellCoordinates(x, y).flatMap {
          case (i, j) =>
            grid.getCellAt(i, j).cell.opt
              .filter(_ => creator.isDefinedAt(newGrid.getCellAt(i, j).cell)) //use the same availability criteria on new grid
              .collect(creator)
              .map((i, j, _))
        }
      if (availableCells.nonEmpty) {
        val (newFireX, newFireY, newCell) = availableCells(random.nextInt(availableCells.size))
        newGrid.setCellAt(newFireX, newFireY, newCell)
        grid.getCellAt(newFireX, newFireY).cell match {
          case cell@HumanCell(_, _) =>
            peopleDeaths += cell.crowd.size
          case _ =>
        }
      }
    }

    def makeMove(x: Int, y: Int): Unit = {
      grid.getCellAt(x, y).cell match {
        case Obstacle =>
          newGrid.setCellAt(x, y, Obstacle)
        case cell@BufferCell(_) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.setCellAt(x, y, cell)
          }
        case EscapeCell(_) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.setCellAt(x, y, EscapeAccessible.unapply(HumanCell.Instance).withEscape())
          }
        case cell: FireCell =>
          if (iteration % config.fireSpeadingFrequency == 0) {
            reproduce(x, y) {
              case FireAccessible(accessible) => accessible.withFire()
            }
          }
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.setCellAt(x, y, cell.copy())
          }
        case cell: HumanCell =>
          newGrid.getCellAt(x, y).cell match {
            case FireCell(_) =>
            case _ => moveHuman(cell, x, y)
          }
      }
    }

    def calculateDirectionVec(cell: HumanCell, x: Int, y: Int, grid: EnhancedGrid): (Signal, Signal) = {
      grid.neighbourCellDirections(x, y)
        .map(dir => toSmellVec(cell, dir))
        .reduce((dest, cur) => (dest._1 + cur._1, dest._2 + cur._2))
    }

    def toSmellVec(cell: HumanCell, direction: Direction): (Signal, Signal) = {
      val smellValue = cell.smell(direction)
      (smellValue * direction.xShift, smellValue * direction.yShift)
    }

    def moveHuman(cell: HumanCell, x: Int, y: Int): Unit = {
      val destinationVec = calculateDirectionVec(cell, x, y, grid)

      val humansStayingInCellAfterMove = cell.crowd
        .filter(human => human.willStayInSameCellAfterMove(destinationVec))
      val humansPassingToAnotherCellAfterMove = cell.crowd
        .filter(human => !human.willStayInSameCellAfterMove(destinationVec))
      val humansArrivedAtCellInThisIteration = newGrid.getCellAt(x, y).cell match {
        case humanCell@HumanCell(_, _) => humanCell.crowd
        case _ => List.empty
      }

      humansStayingInCellAfterMove.foreach(human => human.move(destinationVec))
      newGrid.setCellAt(x, y, cell.copy(smell = cell.smell, crowd = humansStayingInCellAfterMove ++ humansArrivedAtCellInThisIteration))

      humansPassingToAnotherCellAfterMove.foreach(human => moveHumanToAnotherCell(human, x, y, destinationVec))
    }

    def moveHumanToAnotherCell(human: Human, x: Int, y: Int, destinationVec: (Signal, Signal)) = {
      val direction = human.getDestinationDirection(destinationVec)
      val (i, j) = direction.of(x, y)
      val destinationCell = newGrid.getCellAt(i, j).cell
      destinationCell match {
        case humanCell@HumanCell(_, _) => {
          human.move(destinationVec)
          newGrid.setCellAt(i, j, humanCell.copy(smell = humanCell.smell, crowd = humanCell.crowd ++ List(human)))
        }
        case EscapeCell(_) => peopleEscaped += 1
      }
    }

      /*val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)*/
      /*if (cell.crowd.isEmpty) {
        destination match {
          case Opt((i, j, HumanAccessible(destination))) =>
            newGrid.setCellAt(i, j, destination.withHuman(cell.crowd))
            newGrid.getCellAt(i, j).cell match {
              case EscapeCell(_) => peopleEscaped += cell.crowd.size
              case _ =>
            }
          case Opt((i, j, inaccessibleDestination)) =>
            throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
          case Opt.Empty =>
            newGrid.setCellAt(x, y, cell.copy(cell.smell, cell.crowd, cell))
        }
      } else {
        destination match {
          case Opt((i, j, HumanAccessible(destination))) =>
            newGrid.setCellAt(i, j, destination.withHuman(cell.crowd.head.crowd, cell.crowd.head.speed))
            newGrid.setCellAt(x, y, cell.copy(cell.smellWithout(cell.crowd.head.smell), cell.crowd.drop(1), cell.speed))
          case Opt((i, j, inaccessibleDestination)) =>
            throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
          case Opt.Empty =>
            newGrid.setCellAt(x, y, cell.copy(cell.smell, cell.crowd))
        }
      }*/

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      grid.getCellAt(x, y).cell match {
        case HumanCell(_, crowd) =>
          humanCount += crowd.size
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