package pl.edu.agh.torch.continuous.model

import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}

final case class HumanCell(smell: SmellMap, crowd: List[HumanCell], speed: Int)(implicit config: TorchContinuousConfig) extends SmellingCell {

  override type Self = HumanCell

  override def withSmell(smell: SmellMap): HumanCell = copy(smell = smell)
}

trait HumanAccessible[+T <: GridPart] {
  def withHuman(crowd: List[HumanCell], speed: Int): T
}

object HumanAccessible {

  def unapply(arg: EmptyCell)(implicit config: TorchContinuousConfig): HumanAccessible[HumanCell] =
    new HumanAccessible[HumanCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): HumanCell = HumanCell(arg.smellWith(config.humanInitialSignal), crowd, speed)
    }

  def unapply(arg: EscapeCell): HumanAccessible[EscapeCell] =
    new HumanAccessible[EscapeCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): EscapeCell = EscapeCell(arg.smell)
    }

  def unapply(arg: BufferCell)(implicit config: TorchContinuousConfig): HumanAccessible[BufferCell] =
    new HumanAccessible[BufferCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): BufferCell = BufferCell(HumanCell(arg.smellWith(config.humanInitialSignal), crowd, speed))
    }

  def unapply(arg: GridPart)(implicit config: TorchContinuousConfig): Option[HumanAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: EscapeCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
