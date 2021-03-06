package pl.edu.agh.geokin.model.geometry

final case class Line(start: Vec2, end: Vec2) {
  def x1: Double = start.x

  def y1: Double = start.y

  def x2: Double = end.x

  def y2: Double = end.y

  def reversed = new Line(end, start)

  def vector: Vec2 = end - start

  def normal: Vec2 = vector.normal

  def center: Vec2 = (start + end) * 0.5

  def leftOf(p: Vec2): Boolean = (vector cross (p - start)) > 0

  def rightOf(p: Vec2): Boolean = (vector cross (p - start)) <= 0

  def apply(t: Double): Vec2 = start + (vector * t)

  def distance(that: Vec2): Double = Algorithms.distancePointLine(that.x, that.y, x1, y1, x2, y2)

  def segmentDistance(that: Vec2): Double = Algorithms.distancePointLineSegment(that.x, that.y, x1, y1, x2, y2)

  def pointProjection(that: Vec2): Vec2 = Algorithms.projectPointOnLine(that.x, that.y, x1, y1, x2, y2)

  def intersect(that: Line): Option[Algorithms.LineIntersection] = Algorithms.intersect(this, that)

  def intersect(that: Circle): Array[Vec2] = Algorithms.intersectCircleLine(that, this)

  def lengthSq: Double = {
    val dx = start.x - end.x
    val dy = start.y - end.y
    dx * dx + dy * dy
  }

  def length: Double = Math.sqrt(lengthSq)
}

object Line {
  def apply(x1: Double, y1: Double, x2: Double, y2: Double) = new Line(Vec2(x1, y1), Vec2(x2, y2))

  def apply(vec1: Vec2, vec2: Vec2) = new Line(vec1, vec2)
}