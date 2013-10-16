package interval

object Interval {
  private val inclusive = Array("[", "]")
  private val exclusive = Array("(", ")")

  def parse(str: String,
            mark: (String, String),
            f: (Point, Point)=>Interval): Interval = {
    val re = ("\\"+mark._1+"\\s*(.+)\\s*,\\s*(.+)\\s*\\"+mark._2).r
    str.trim match {
      case re(x,y) => 
        val l = if(x.forall(_.isDigit)) Integer.parseInt(x): Point
                else if (x.equals("-inf")) mInfinite
                else x: Point
        val u = if(y.forall(_.isDigit)) Integer.parseInt(y): Point
                else if (y.equals("+inf")) pInfinite
                else y: Point
        f(l, u)
      case _ => throw new IntervalException("invalid notation")
    }
  }

  def filter[T: PointType](seq: Seq[T], interval: Interval): Set[T] = {
    val imp = implicitly[PointType[T]]
    imp.unconvSet(imp.convSeq(seq).filter(interval.contains(_)).toSet)
  }
}

abstract class Interval(val leftEnd: String, val rightEnd: String) {

  def lowerPoint: Point
  def upperPoint: Point

  private def max(p1: Point, p2: Point): Point = if(p1.less(p2)) p2 else p1
  private def min(p1: Point, p2: Point): Point = if(p1.less(p2)) p1 else p2

  if((lowerPoint==pInfinite && upperPoint!=pInfinite) ||
     (lowerPoint==mInfinite && isLeftInclusive) ||
     (upperPoint==mInfinite && lowerPoint!=mInfinite) ||
     (upperPoint==pInfinite && isRightInclusive) ||
     (lowerPoint!=mInfinite && upperPoint!=pInfinite &&
      !lowerPoint.less(upperPoint)))
    throw new IntervalException(
        "%s should be lower than %s".format(lowerPoint, upperPoint)
    );

  def isLeftInclusive = leftEnd == "["
  def isRightInclusive = rightEnd == "]"

  def contains(p: Point): Boolean = 
    (!lowerPoint.greater(p) ||
     (isLeftInclusive && lowerPoint == (p))) &&
    (!(p).greater(upperPoint) ||
     (isRightInclusive && upperPoint == (p)))

  def containsAll[T: PointType](arg: Seq[T]) =
    implicitly[PointType[T]].convSeq(arg).forall(this.contains(_))

  def isConnectedTo(other: Interval) =
    (!(upperPoint.less(other.lowerPoint)) ||
     (isRightInclusive && other.isLeftInclusive && 
      upperPoint == other.lowerPoint)) &&
    (!(other.upperPoint.less(lowerPoint)) ||
     (other.isRightInclusive && isLeftInclusive && 
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
        if(lowerPoint == mInfinite && other.lowerPoint == mInfinite)
          (false, mInfinite)
        else if(lowerPoint == mInfinite)
          (other.isLeftInclusive, other.lowerPoint)
        else if(other.lowerPoint == mInfinite)
          (isLeftInclusive, lowerPoint)
        else if(!lowerPoint.greater(other.lowerPoint))
          (other.isLeftInclusive,
	   max(lowerPoint, other.lowerPoint))
        else if(lowerPoint == other.lowerPoint)
          (other.isLeftInclusive && isLeftInclusive,
	   max(lowerPoint, other.lowerPoint))
        else
          (isLeftInclusive,
	   max(lowerPoint, other.lowerPoint))

      val right =
        if(upperPoint == pInfinite && other.upperPoint == pInfinite)
          (false, pInfinite)
        else if(upperPoint == pInfinite)
          (other.isRightInclusive, other.upperPoint)
        else if(other.upperPoint == pInfinite)
          (isRightInclusive, upperPoint)
        else if(!upperPoint.less(other.upperPoint))
          (other.isRightInclusive,
	   min(upperPoint, other.upperPoint))
        else if(upperPoint == other.upperPoint)
          (other.isRightInclusive && isRightInclusive,
	   min(upperPoint, other.upperPoint))
        else
          (isRightInclusive,
	   min(upperPoint, other.upperPoint))

      (left, right) match {
        case ((true, lp), (true, up)) => ClosedInterval(lp, up)
        case ((true, lp), (false, up)) => ClosedOpenInterval(lp, up)
        case ((false, lp), (true, up)) => OpenClosedInterval(lp, up)
        case ((false, lp), (false, up)) => OpenInterval(lp, up)
      }
    }

  override def toString: String =
    (leftEnd+"%s,%s"+rightEnd).format(lowerPoint.toString, upperPoint.toString)
}
