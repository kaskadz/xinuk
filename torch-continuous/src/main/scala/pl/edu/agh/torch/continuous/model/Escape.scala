package pl.edu.agh.torch.continuous.model

import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.{EmptyCell, GridPart, SmellingCell}

final case class EscapeCell(smell: SmellMap) extends SmellingCell {
  override type Self = EscapeCell

  override def withSmell(smell: SmellMap): EscapeCell = copy(smell = smell)
}

trait EscapeAccessible[+T <: GridPart] {
  def withEscape(): T
}

object EscapeAccessible {

  def unapply(arg: EmptyCell)(implicit config: TorchContinuousConfig): EscapeAccessible[EscapeCell] =
    new EscapeAccessible[EscapeCell] {
      override def withEscape(): EscapeCell = EscapeCell(arg.smellWith(config.escapeInitialSignal))
    }

  def unapply(arg: GridPart)(implicit config: TorchContinuousConfig): Option[EscapeAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case _ => None
  }
}
