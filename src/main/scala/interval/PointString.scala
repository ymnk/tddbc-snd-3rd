package interval

class PointString(_point: String) extends Point {
  def point(): String = _point
  override def toString() = point.toString
  override def equals(other: Any) = other match {
    case that: PointString => point == that.point
    case _ => super.equals(other)
  }
  override def less(other: Point) = other match {
    case that: PointString => point <= that.point
    case _ => super.less(other)
  }
  override def greater(other: Point) = other match {
    case that: PointString => point >= that.point
    case _ => super.greater(other)
  }
}
