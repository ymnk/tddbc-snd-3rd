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
                else throw new IntervalException("invalid notation")
        val u = if(y.forall(_.isDigit)) Integer.parseInt(y): Point
                else if (y.equals("+inf")) pInfinite
                else throw new IntervalException("invalid notation")
        f(l, u)
      case _ => throw new IntervalException("invalid notation")
    }
  }

  def filter(seq: Seq[Int], interval: Interval): Set[Int] = {
    seq.filter(interval.contains(_)).toSet
  }
}

abstract class Interval(val leftEnd: String, val rightEnd: String) {

  def lowerPoint: Point
  def upperPoint: Point

  if((lowerPoint==pInfinite && upperPoint!=pInfinite) ||
     (lowerPoint==mInfinite && isLeftInclusive) ||
     (upperPoint==mInfinite && lowerPoint!=mInfinite) ||
     (upperPoint==pInfinite && isRightInclusive) ||
     (lowerPoint!=mInfinite && upperPoint!=pInfinite &&
      lowerPoint.point > upperPoint.point))
    throw new IntervalException(
        "%s should be lower than %s".format(lowerPoint, upperPoint)
    );

  def isLeftInclusive = leftEnd == "["
  def isRightInclusive = rightEnd == "]"

  def contains(p: Int): Boolean = 
    if(lowerPoint == mInfinite && upperPoint == pInfinite) true
    else if(lowerPoint == mInfinite) {
      (p < upperPoint.point || (isRightInclusive && upperPoint.point == p))
    }
    else if(upperPoint == pInfinite) {
      (lowerPoint.point < p || (isLeftInclusive && lowerPoint.point == p))
    }
    else  
      ((lowerPoint.point < p || (isLeftInclusive && lowerPoint.point == p)) &&
       (p < upperPoint.point || (isRightInclusive && upperPoint.point == p)))

  def containsAll(arg: Seq[Int]) = arg.forall(this.contains(_))

  def isConnectedTo(other: Interval) =
    if(lowerPoint == mInfinite && upperPoint == pInfinite) true
    else if(lowerPoint == mInfinite) {
      (upperPoint.point > other.lowerPoint.point ||
       (isRightInclusive && other.isLeftInclusive && 
        upperPoint.point == other.lowerPoint.point))
    } 
    else if(upperPoint == pInfinite) {
      (other.upperPoint.point > lowerPoint.point ||
       (other.isRightInclusive && isLeftInclusive && 
        other.upperPoint.point == lowerPoint.point))
    }
    else {
      (upperPoint.point > other.lowerPoint.point ||
       (isRightInclusive && other.isLeftInclusive && 
        upperPoint.point == other.lowerPoint.point)) &&
      (other.upperPoint.point > lowerPoint.point ||
       (other.isRightInclusive && isLeftInclusive && 
        other.upperPoint.point == lowerPoint.point))
    }

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
          false
        else if(lowerPoint == mInfinite)
          other.isLeftInclusive
        else if(other.lowerPoint == mInfinite)
          isLeftInclusive
        else if(lowerPoint.point < other.lowerPoint.point)
          other.isLeftInclusive
        else if(lowerPoint.point == other.lowerPoint.point)
          other.isLeftInclusive && isLeftInclusive
        else
          isLeftInclusive

      val right =
        if(upperPoint == pInfinite && other.upperPoint == pInfinite)
          false
        else if(upperPoint == pInfinite)
          other.isRightInclusive
        else if(other.upperPoint == pInfinite)
          isRightInclusive
        else if(upperPoint.point > other.upperPoint.point)
          other.isRightInclusive
        else if(upperPoint.point == other.upperPoint.point)
          other.isRightInclusive && isRightInclusive
        else
          isRightInclusive

      val lp: Point = 
        if(lowerPoint == mInfinite && other.lowerPoint == mInfinite)
          mInfinite
        else if(lowerPoint == mInfinite)
          other.lowerPoint
        else if(other.lowerPoint == mInfinite)
          lowerPoint
        else Math.max(lowerPoint.point, other.lowerPoint.point):Point

      val up: Point =
        if(upperPoint == pInfinite && other.upperPoint == pInfinite)
          pInfinite
        else if(upperPoint == pInfinite)
          other.upperPoint
        else if(other.upperPoint == pInfinite)
          upperPoint
        else Math.min(upperPoint.point, other.upperPoint.point):Point

      (left, right) match {
        case (true, true) => ClosedInterval(lp, up)
        case (true, false) => ClosedOpenInterval(lp, up)
        case (false, true) => OpenClosedInterval(lp, up)
        case (false, false) => OpenInterval(lp, up)
      }
    }

  override def toString: String =
    (leftEnd+"%s,%s"+rightEnd).format(lowerPoint.toString, upperPoint.toString)
}
