package interval

object ClosedInterval {
  val mark = ("[", "]")
  def parse(str: String): ClosedInterval = Interval.parse(str, mark) match {
    case (x, y) => 
      ClosedInterval(Integer.parseInt(x), Integer.parseInt(y))
  }
}

case class ClosedInterval(val lowerPoint: Int, val upperPoint: Int)
  extends Interval(ClosedInterval.mark._1, ClosedInterval.mark._2) {
} 
