package interval

class ClosedInterval(val lowerPoint: Int, val upperPoint: Int) {
  if(lowerPoint > upperPoint)
    throw new IntervalException(
      "%d should be lower than %d".format(lowerPoint, upperPoint)
    );

  def contains(p: Int): Boolean = 
    (lowerPoint <= p && p <= upperPoint)

  override def equals(other:Any) = other match {
    case that: ClosedInterval =>
     lowerPoint == that.lowerPoint && upperPoint == that.upperPoint
    case _ => false
  }

  override def toString: String =
    "[%d,%d]".format(lowerPoint, upperPoint)
} 
