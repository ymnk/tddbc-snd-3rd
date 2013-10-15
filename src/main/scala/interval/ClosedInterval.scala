package interval

object ClosedInterval {
  val mark = ("[", "]")
  def parse(str: String): Interval =
     Interval.parse(str, mark, ClosedInterval.apply _)
}

case class ClosedInterval(val lowerPoint: Point, val upperPoint: Point)
  extends Interval(ClosedInterval.mark._1, ClosedInterval.mark._2) {
} 
