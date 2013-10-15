package interval

object OpenClosedInterval {
  val mark = ("(", "]")
  def parse(str: String): Interval =
    Interval.parse(str, mark, OpenClosedInterval.apply _)
}

case class OpenClosedInterval(val lowerPoint: Point, val upperPoint: Point)
  extends Interval(OpenClosedInterval.mark._1, OpenClosedInterval.mark._2) {
} 
