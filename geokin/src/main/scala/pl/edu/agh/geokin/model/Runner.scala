package pl.edu.agh.geokin.model

import pl.edu.agh.geokin.model.geometry.Vec2


final case class Runner(position: Vec2, radius: Double, heading: Double, speed: Double) {
  def makeMove(): Unit = ???
}
