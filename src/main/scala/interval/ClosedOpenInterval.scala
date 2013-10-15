package interval

object ClosedOpenInterval {
  private val re = "\\[\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\)".r
  def parse(str: String): ClosedOpenInterval = str.trim match {
    case re(x,y) =>
      new ClosedOpenInterval(Integer.parseInt(x), Integer.parseInt(y))
    case _ => throw new IntervalException("invalid notation")
  }
}

class ClosedOpenInterval(lower: Int, upper: Int) extends Interval(lower, upper) {

  def contains(p: Int): Boolean = 
    (lowerPoint <= p && p < upperPoint)

  override def equals(other:Any) = other match {
    case that: ClosedOpenInterval =>
     lowerPoint == that.lowerPoint && upperPoint == that.upperPoint
    case _ => false
  }

  def isConnectedTo(other: Interval) = other match {
    case that: ClosedInterval =>
      !(upperPoint <= that.lowerPoint) && !(that.upperPoint < lowerPoint)
    case that: OpenInterval =>
      !(upperPoint <= other.lowerPoint) && !(other.upperPoint <= lowerPoint)
    case that: ClosedOpenInterval =>
      !(upperPoint <= other.lowerPoint) && !(other.upperPoint <= lowerPoint)
    case _ => false
  }

  override def toString: String =
    "[%d,%d)".format(lowerPoint, upperPoint)
} 
