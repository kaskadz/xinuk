package pl.edu.agh.torch.continuous.model

import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.{BufferCell, Cell, GridPart, SmellingCell}

final case class HumanCell(smell: SmellMap, crowd: List[Human]) extends SmellingCell {
  override type Self = HumanCell

  override def withSmell(smell: SmellMap): HumanCell = copy(smell = smell)
}

object HumanCell {
  final val Instance: HumanCell = HumanCell(Cell.emptySignal, List.empty)
}

class Human(var x: Double, var y: Double)(implicit config: TorchContinuousConfig) {

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
