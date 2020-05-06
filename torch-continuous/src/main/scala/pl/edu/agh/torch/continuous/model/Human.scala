package pl.edu.agh.torch.continuous.model

import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.Direction.Direction
import pl.edu.agh.xinuk.model.{BufferCell, Cell, Direction, GridPart, Signal, SmellingCell}

final case class HumanCell(smell: SmellMap, crowd: List[Human]) extends SmellingCell {
  override type Self = HumanCell

  override def withSmell(smell: SmellMap): HumanCell = copy(smell = smell)
}

object HumanCell {
  final val Instance: HumanCell = HumanCell(Cell.emptySignal, List.empty)
}

class Human(var x: Double, var y: Double, val speed: Double)(implicit config: TorchContinuousConfig) {

  def move(direction: (Signal, Signal)): Unit = {
    x = move(x, direction._1.value * speed)
    y = move(y, direction._2.value * speed)
  }

  private def move(axis: Double, value: Double): Double = {
    if (axis + value < 0) {
      config.cellSize - (axis + value) % config.cellSize
    } else {
      (axis + value) % config.cellSize
    }
  }

  def willStayInSameCellAfterMove(direction: (Signal, Signal)): Boolean = {
    val newX = x + direction._1.value * speed
    val newY = y + direction._2.value * speed
    newX >= 0 && newX < config.cellSize && newY >= 0 && newY < config.cellSize
  }

  def getDestinationDirection(direction: (Signal, Signal)): Direction = {
    val newX = x + direction._1.value * speed
    val newY = y + direction._2.value * speed
    val dirX = getDestinationDirection(newX)
    val dirY = getDestinationDirection(newY)
    Direction.values.filter(dir => dir.xShift == dirX && dir.yShift == dirY).head
  }

  private def getDestinationDirection(newCoordinate: Double): Int = {
    if (newCoordinate > config.cellSize) {
      1
    } else if (newCoordinate < 0) {
      -1
    } else {
      0
    }
  }
}

trait HumanAccessible[+T <: GridPart] {
  def withHuman(crowd: List[Human]): T
}

object HumanAccessible {

  def unapply(arg: HumanCell)(implicit config: TorchContinuousConfig): HumanAccessible[HumanCell] =
    new HumanAccessible[HumanCell] {
      override def withHuman(crowd: List[Human]): HumanCell = HumanCell(arg.smellWith(config.humanInitialSignal), arg.crowd ++ crowd)
    }

  def unapply(arg: EscapeCell): HumanAccessible[EscapeCell] =
    new HumanAccessible[EscapeCell] {
      override def withHuman(crowd: List[Human]): EscapeCell = EscapeCell(arg.smell)
    }

  def unapply(arg: BufferCell)(implicit config: TorchContinuousConfig): HumanAccessible[BufferCell] =
    new HumanAccessible[BufferCell] {
      override def withHuman(crowd: List[Human]): BufferCell = BufferCell(HumanCell(arg.smellWith(config.humanInitialSignal), crowd))
    }

  def unapply(arg: GridPart)(implicit config: TorchContinuousConfig): Option[HumanAccessible[GridPart]] = arg match {
    case cell: HumanCell => Some(unapply(cell))
    case cell: EscapeCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
