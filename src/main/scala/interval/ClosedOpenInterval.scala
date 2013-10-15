package interval

object ClosedOpenInterval {
  val mark = ("[", ")")
  def parse(str: String): ClosedOpenInterval =
    Interval.parse(str, mark) match {
      case (x, y) =>
        ClosedOpenInterval(Integer.parseInt(x), Integer.parseInt(y))
    }
}

case class ClosedOpenInterval(val lowerPoint: Int, val upperPoint: Int)
  extends Interval(ClosedOpenInterval.mark._1, ClosedOpenInterval.mark._2) {
} 