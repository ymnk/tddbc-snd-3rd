package interval

object OpenInterval {
  private val re = "\\(\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\)".r
  def parse(str: String): OpenInterval = str.trim match {
    case re(x,y) =>
      new OpenInterval(Integer.parseInt(x), Integer.parseInt(y))
    case _ => throw new IntervalException("invalid notation")
  }
}

class OpenInterval(lower: Int, upper: Int) extends Interval(lower, upper) {

  def contains(p: Int): Boolean = 
    (lowerPoint < p && p < upperPoint)

  override def equals(other:Any) = other match {
    case that: OpenInterval =>
     lowerPoint == that.lowerPoint && upperPoint == that.upperPoint
    case _ => false
  }

  def isConnectedTo(other: Interval) =
      !(upperPoint <= other.lowerPoint) && !(other.upperPoint <= lowerPoint)

  override def toString: String =
    "(%d,%d)".format(lowerPoint, upperPoint)
} 
