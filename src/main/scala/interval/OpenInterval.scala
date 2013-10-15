package interval

object OpenInterval {
  private val re = "\\(\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\)".r
  def parse(str: String): OpenInterval = str.trim match {
    case re(x,y) =>
      new OpenInterval(Integer.parseInt(x), Integer.parseInt(y))
    case _ => throw new IntervalException("invalid notation")
  }
}

case class OpenInterval(val lowerPoint: Int, val upperPoint: Int) extends Interval {

  def contains(p: Int): Boolean = 
    (lowerPoint < p && p < upperPoint)

  def isConnectedTo(other: Interval) =
      !(upperPoint <= other.lowerPoint) && !(other.upperPoint <= lowerPoint)

  override def toString: String =
    "(%d,%d)".format(lowerPoint, upperPoint)
} 
