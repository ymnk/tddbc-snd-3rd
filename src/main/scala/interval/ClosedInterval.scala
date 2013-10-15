package interval

object ClosedInterval {
  val re = "\\[\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\]".r
  def parse(str: String): ClosedInterval = str.trim match {
    case re(x,y)  =>
      new ClosedInterval(Integer.parseInt(x), Integer.parseInt(y))
    case _ => throw new IntervalException("invalid notation")
  }
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
