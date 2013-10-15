package interval

object OpenClosedInterval {
  val mark = ("(", "]")
  def parse(str: String): OpenClosedInterval =
    Interval.parse(str, mark) match {
      case (x, y) =>
        OpenClosedInterval(Integer.parseInt(x), Integer.parseInt(y))
    }
}

case class OpenClosedInterval(val lowerPoint: Int, val upperPoint: Int) extends Interval {
  import OpenClosedInterval.mark
  val leftEnd = mark._1
  val rightEnd = mark._2

  def isConnectedTo(other: Interval) = other match {
    case ClosedInterval(l, u) => !(upperPoint < l) && !(u <= lowerPoint)
    case OpenInterval(l, u) => !(upperPoint <= l) && !(u <= lowerPoint)
    case ClosedOpenInterval(l, u) => !(upperPoint <= l) && !(u <= lowerPoint)
    case OpenClosedInterval(l, u) => !(upperPoint <= l) && !(u <= lowerPoint)
    case _ => false
  }
} 
