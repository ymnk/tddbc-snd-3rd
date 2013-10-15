package interval

object ClosedOpenInterval {
  val mark = ("[", ")")
  def parse(str: String): Interval =
    Interval.parse(str, mark, ClosedOpenInterval.apply _)
}

case class ClosedOpenInterval(val lowerPoint: Point, val upperPoint: Point)
  extends Interval(ClosedOpenInterval.mark._1, ClosedOpenInterval.mark._2) {
} 
