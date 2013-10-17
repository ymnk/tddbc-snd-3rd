package interval

class PointInt(_point: Int) extends Point {
  def point(): Int = _point
  override def toString() = point.toString
  override def equals(other: Any) = other match {
    case that: PointInt => point == that.point
    case _ => super.equals(other)
  }
  override def lessEq(other: Point) = other match {
    case that: PointInt => point <= that.point
    case _ => super.lessEq(other)
  }
  override def greaterEq(other: Point) = other match {
    case that: PointInt => point >= that.point
    case _ => super.greaterEq(other)
  }
}
