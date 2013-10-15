package interval

object OpenInterval {
  val mark = ("(", ")")
  def parse(str: String): Interval =
    Interval.parse(str, mark, OpenInterval.apply _)
}

case class OpenInterval(val lowerPoint: Point, val upperPoint: Point)
  extends Interval(OpenInterval.mark._1, OpenInterval.mark._2) {
} 
