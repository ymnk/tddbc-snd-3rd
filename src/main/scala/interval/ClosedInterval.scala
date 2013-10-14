package interval

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

  override def toString: String =
    "[%d,%d]".format(lowerPoint, upperPoint)
} 
