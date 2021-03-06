package pl.edu.agh.geokin.model.geometry

final case class Circle(center: Vec2, r: Double) {
  def x: Double = center.x
  def y: Double = center.y
  def d: Double = r * 2

  def circumferencePoint(angle: Double): Vec2 = center + (Vec2.unit(angle) * r)

  def outerTangentCW(that: Circle): Option[Line] = Algorithms.circleOuterTangentCW(this, that)
  def outerTangentCCW(that: Circle): Option[Line] = Algorithms.circleOuterTangentCW(that, this).map(_.reversed)

  def centersDistance(that: Circle): Double = Line.apply(this.center, that.center).length

  def includes(that: Circle): Boolean = {
    val centersDistance = this.centersDistance(that)
    val rDiff = Math.abs(this.r - that.r)

    centersDistance < rDiff
  }

  def intersects(that: Circle): Boolean = {
    val rDiff = Math.abs(this.r - that.r)
    val rSum = this.r + that.r
    val centersDistance = this.centersDistance(that)

    centersDistance < rDiff && rDiff < rSum
  }
}
