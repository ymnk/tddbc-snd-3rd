package interval

object ClosedInterval {
  val mark = ("[", "]")
  def parse(str: String): ClosedInterval = Interval.parse(str, mark) match {
    case (x, y) => 
      ClosedInterval(Integer.parseInt(x), Integer.parseInt(y))
  }
}

case class ClosedInterval(val lowerPoint: Int, val upperPoint: Int) extends Interval {
  import ClosedInterval.mark
  val leftEnd = mark._1
  val rightEnd = mark._2

  def isConnectedTo(other: Interval) = other match {
    case ClosedInterval(l, u) => !(upperPoint < l) && !(u < lowerPoint)
    case OpenInterval(l, u) => !(upperPoint <= l) && !(u <= lowerPoint)
    case _ => false
  }

  def getIntersection(interval: ClosedInterval) = 
    if(!isConnectedTo(interval))
      throw new IntervalException(
        "there is no the intersection between %s and %s".
          format(toString(), interval.toString())
      )
    else
      new ClosedInterval(Math.max(lowerPoint, interval.lowerPoint),
                         Math.min(upperPoint, interval.upperPoint))
} 
