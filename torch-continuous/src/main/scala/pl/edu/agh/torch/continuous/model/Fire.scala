package pl.edu.agh.torch.continuous.model

import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.{BufferCell, GridPart, SmellingCell}


final case class FireCell(smell: SmellMap) extends SmellingCell {
  override type Self = FireCell

  override def withSmell(smell: SmellMap): FireCell = copy(smell = smell)
}

trait FireAccessible[+T <: GridPart] {
  def withFire(): T
}

object FireAccessible {
  def unapply(arg: HumanCell)(implicit config: TorchContinuousConfig): FireAccessible[FireCell] =
    new FireAccessible[FireCell] {
      override def withFire(): FireCell = FireCell(arg.smellWith(config.fireInitialSignal))
    }

  def unapply(arg: EscapeCell)(implicit config: TorchContinuousConfig): FireAccessible[FireCell] =
    new FireAccessible[FireCell] {
      override def withFire(): FireCell = FireCell(arg.smellWith(config.fireInitialSignal))
    }

  def unapply(arg: BufferCell)(implicit config: TorchContinuousConfig): FireAccessible[BufferCell] =
    new FireAccessible[BufferCell] {
      override def withFire(): BufferCell = BufferCell(FireCell(arg.smellWith(config.fireInitialSignal)))
    }

  def unapply(arg: GridPart)(implicit config: TorchContinuousConfig): Option[FireAccessible[GridPart]] = arg match {
    case cell: EscapeCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case cell: HumanCell => Some(unapply(cell))
    case _ => None
  }
}
