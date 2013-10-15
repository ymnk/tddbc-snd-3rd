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
