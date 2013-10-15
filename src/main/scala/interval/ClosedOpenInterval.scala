package interval

object ClosedOpenInterval {
  private val re = "\\[\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\)".r
  def parse(str: String): ClosedOpenInterval = str.trim match {
    case re(x,y) =>
      new ClosedOpenInterval(Integer.parseInt(x), Integer.parseInt(y))
    case _ => throw new IntervalException("invalid notation")
  }
}

case class ClosedOpenInterval(val lowerPoint: Int, val upperPoint: Int) extends Interval {

  def contains(p: Int): Boolean = 
    (lowerPoint <= p && p < upperPoint)

  def isConnectedTo(other: Interval) = other match {
    case ClosedInterval(l, u) => !(upperPoint <= l) && !(u < lowerPoint)
    case OpenInterval(l, u) => !(upperPoint <= l) && !(u <= lowerPoint)
    case ClosedOpenInterval(l, u) => !(upperPoint <= l) && !(u <= lowerPoint)
    case _ => false
  }

  override def toString: String =
    "[%d,%d)".format(lowerPoint, upperPoint)
} 
