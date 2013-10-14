package interval

object ClosedInterval {
  def parse(str: String) = new ClosedInterval(0, 0)
}

class ClosedInterval(lower: Int, upper: Int) extends Interval(lower, upper) {

  def contains(p: Int): Boolean = 
    (lowerPoint <= p && p <= upperPoint)

  override def equals(other:Any) = other match {
    case that: ClosedInterval =>
     lowerPoint == that.lowerPoint && upperPoint == that.upperPoint
    case _ => false
  }

  def isConnectedTo(other: Interval) = other match {
    case that: ClosedInterval =>
      !(upperPoint < that.lowerPoint) && !(that.upperPoint < lowerPoint)
    case that: OpenInterval =>
      !(upperPoint <= that.lowerPoint) && !(that.upperPoint <= lowerPoint)
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

  override def toString: String =
    "[%d,%d]".format(lowerPoint, upperPoint)
} 
