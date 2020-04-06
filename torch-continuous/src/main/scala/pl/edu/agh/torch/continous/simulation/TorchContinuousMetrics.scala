package pl.edu.agh.torch.continous.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class TorchContinuousMetrics(peopleCount: Long,
                                        fireCount: Long,
                                        escapeCount: Long,
                                        peopleDeaths: Long,
                                        peopleEscaped: Long) extends Metrics {
  override def log: String = {
    s"$peopleCount;$fireCount;$escapeCount;$peopleDeaths;$peopleEscaped"
  }

  override def series: Vector[(String, Double)] = Vector(
    "People" -> peopleCount,
    "Fire" -> fireCount,
    "Escape" -> escapeCount,
    "PeopleDeaths" -> peopleDeaths
  )

  override def +(other: Metrics): TorchContinuousMetrics = {
    other match {
      case TorchContinuousMetrics.EMPTY => this
      case TorchContinuousMetrics(otherPeopleCount, otherFireCount, otherEscapeCount, otherPeopleDeaths, otherPeopleEscaped) =>
        TorchContinuousMetrics(peopleCount + otherPeopleCount, fireCount + otherFireCount, escapeCount + otherEscapeCount,
          peopleDeaths + otherPeopleDeaths, peopleEscaped + otherPeopleEscaped)
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-TorchMetrics to TorchMetrics")
    }
  }
}

object TorchContinuousMetrics {
  private val EMPTY = TorchContinuousMetrics(0, 0, 0, 0, 0)

  def empty(): TorchContinuousMetrics = EMPTY
}
