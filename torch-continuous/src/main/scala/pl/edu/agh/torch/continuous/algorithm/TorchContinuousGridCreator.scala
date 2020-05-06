package pl.edu.agh.torch.continuous.algorithm

import pl.edu.agh.torch.continuous.config.TorchContinuousConfig
import pl.edu.agh.torch.continuous.model.{EscapeAccessible, FireAccessible, Human, HumanAccessible, HumanCell}
import pl.edu.agh.xinuk.algorithm.GridCreator
import pl.edu.agh.xinuk.model.{Grid, NonPlanarConnections}

import scala.util.Random

final class TorchContinuousGridCreator(implicit config: TorchContinuousConfig) extends GridCreator {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, NonPlanarConnections) = {
    val grid = Grid.empty{ HumanCell.Instance }

    var humanCount = 0L
    var fireCount = 0L
    var escapesCount = 0L
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
    } {
      if (random.nextDouble() < config.spawnChance) {
        grid.cells(x)(y) =
          random.nextInt(3) match {
            case 0 =>
              if (random.nextDouble() < config.humanSpawnChance) {
                humanCount += 1
                val humanX = random.nextInt(config.cellSize)
                val humanY = random.nextInt(config.cellSize)
                val humanSpeed = random.nextInt(config.humanMaxSpeed - config.humanMinSpeed) + config.humanMinSpeed
                val human = new Human(humanX, humanY, humanSpeed)
                HumanAccessible.unapply(HumanCell.Instance).withHuman(List(human))
              } else {
                grid.cells(x)(y)
              }
            case 1 =>
              if (random.nextDouble() < config.escapeSpawnChance) {
                escapesCount += 1
                EscapeAccessible.unapply(HumanCell.Instance).withEscape()
              } else {
                grid.cells(x)(y)
              }
            case 2 =>
              if (random.nextDouble() < config.fireSpawnChance) {
                fireCount += 1
                FireAccessible.unapply(HumanCell.Instance).withFire()
              } else {
                grid.cells(x)(y)
              }
          }
      }
    }

    (grid, NonPlanarConnections(Set()))
  }
}

object TorchContinuousGridCreator {
  def apply(implicit config: TorchContinuousConfig): TorchContinuousGridCreator = new TorchContinuousGridCreator
}
