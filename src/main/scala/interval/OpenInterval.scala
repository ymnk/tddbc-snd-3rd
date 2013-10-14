package interval

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
    "[%d,%d]".format(lowerPoint, upperPoint)
} 
