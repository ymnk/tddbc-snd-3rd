package interval

object ClosedInterval {
  val re = "\\[\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\]".r
  def parse(str: String): ClosedInterval = str.trim match {
    case re(x,y)  =>
      new ClosedInterval(Integer.parseInt(x), Integer.parseInt(y))
    case _ => throw new IntervalException("invalid notation")
  }
}

case class ClosedInterval(val lowerPoint: Int, val upperPoint: Int) extends Interval {

  def contains(p: Int): Boolean = 
    (lowerPoint <= p && p <= upperPoint)

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

  override def toString: String =
    "[%d,%d]".format(lowerPoint, upperPoint)
} 
