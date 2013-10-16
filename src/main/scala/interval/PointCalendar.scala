package interval

class PointCalendar(_point: java.util.Calendar) extends Point {
  import java.util.Calendar
  import java.util.Calendar.{YEAR, MONTH, DATE}
  def point(): java.util.Calendar = _point
  override def toString() =
    "%d/%d/%d".format(point.get(YEAR),point.get(MONTH)+1,point.get(DATE))
  override def equals(other: Any) = other match {
    case that: PointCalendar =>
      point.getTimeInMillis == that.point.getTimeInMillis
    case _ => super.equals(other)
  }
  override def less(other: Point) = other match {
    case that: PointCalendar =>
      point.getTimeInMillis <= that.point.getTimeInMillis
    case _ => super.less(other)
  }
  override def greater(other: Point) = other match {
    case that: PointCalendar =>
      point.getTimeInMillis >= that.point.getTimeInMillis
    case _ => super.greater(other)
  }
}
