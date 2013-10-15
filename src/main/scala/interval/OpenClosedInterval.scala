package interval

object OpenClosedInterval {
  val mark = ("(", "]")
  def parse(str: String): OpenClosedInterval =
    Interval.parse(str, mark) match {
      case (x, y) =>
        OpenClosedInterval(Integer.parseInt(x), Integer.parseInt(y))
    }
}

case class OpenClosedInterval(val lowerPoint: Int, val upperPoint: Int)
  extends Interval(OpenClosedInterval.mark._1, OpenClosedInterval.mark._2) {
} 
