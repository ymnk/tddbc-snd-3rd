package interval

object OpenInterval {
  val mark = ("(", ")")
  def parse(str: String): OpenInterval = Interval.parse(str, mark) match {
    case (x, y) => 
      OpenInterval(Integer.parseInt(x), Integer.parseInt(y))
  }
}

case class OpenInterval(val lowerPoint: Int, val upperPoint: Int)
  extends Interval(OpenInterval.mark._1, OpenInterval.mark._2) {
} 
