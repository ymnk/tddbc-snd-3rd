package interval

object Interval {
  private val inclusive = Array("[", "]")
  private val exclusive = Array("(", ")")
  def isInclusive(arg: String) = inclusive.contains(arg)

  def parse(str: String, mark: (String, String)): (String, String) = {
    val re = ("\\"+mark._1+"\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\"+mark._2).r
    str.trim match {
      case re(x,y)  => (x, y)
      case _ => throw new IntervalException("invalid notation")
    }
  }
}

abstract class Interval(val leftEnd: String, val rightEnd: String) {
  import Interval.isInclusive

  def lowerPoint: Int
  def upperPoint: Int

  if(lowerPoint > upperPoint)
    throw new IntervalException(
      "%d should be lower than %d".format(lowerPoint, upperPoint)
    );

  def isLeftInclusive = leftEnd == "["
  def isRightInclusive = rightEnd == "]"

  def contains(p: Int): Boolean = 
    (lowerPoint < p || (isInclusive(leftEnd) && lowerPoint == p)) &&
      (p < upperPoint || (isInclusive(rightEnd) && upperPoint == p))

  def containsAll(arg: Seq[Int]) = arg.forall(this.contains(_))

  def isConnectedTo(other: Interval) =
      (upperPoint > other.lowerPoint ||
       (isInclusive(rightEnd) && isInclusive(other.leftEnd) && 
        upperPoint == other.lowerPoint)) &&
      (other.upperPoint > lowerPoint ||
       (isInclusive(other.rightEnd) && isInclusive(leftEnd) && 
        other.upperPoint == lowerPoint))

  def getIntersection(other: Interval) = 
    if(!isConnectedTo(other)){
      throw new IntervalException(
        "there is no intersection between %s and %s".
          format(toString(), other.toString())
      )
    }
    else {
      val left =
        if(lowerPoint < other.lowerPoint)
          other.isLeftInclusive
        else if(lowerPoint == other.lowerPoint)
          other.isLeftInclusive && isLeftInclusive
        else
          isLeftInclusive

      val right =
        if(upperPoint > other.upperPoint)
          other.isRightInclusive
        else if(upperPoint == other.upperPoint)
          other.isRightInclusive && isRightInclusive
        else
          isRightInclusive

      val rp = Math.max(lowerPoint, other.lowerPoint)
      val lp = Math.min(upperPoint, other.upperPoint)

      (left, right) match {
        case (true, true) => ClosedInterval(rp, lp)
        case (true, false) => ClosedOpenInterval(rp, lp)
        case (false, true) => OpenClosedInterval(rp, lp)
        case (false, false) => OpenInterval(rp, lp)
      }
    }

  override def toString: String =
    (leftEnd+"%d,%d"+rightEnd).format(lowerPoint, upperPoint)
}
