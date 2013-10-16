package interval

object Interval {

  private val parsers =
    List(ClosedInterval.parse _, 
         ClosedOpenInterval.parse _, 
         OpenInterval.parse _, 
         OpenClosedInterval.parse _)
  def parse(str: String): Interval = {
    parsers.foldLeft(None: Option[Interval]){
     case (Some(s), _) => Some(s)
     case (_ , e) => try{ Some(e(str)) }catch{case e: Throwable => None}
    }.getOrElse{ throw new IntervalException("invalid notation") }
  }

  def parse(str: String,
            mark: (String, String),
            f: (Point, Point)=>Interval): Interval = {
    val re = ("\\"+mark._1+"\\s*(.+)\\s*,\\s*(.+)\\s*\\"+mark._2).r
    val re_date = ("\\"+mark._1+"\\s*(\\d+)/(\\d+)/(\\d+)\\s*,\\s*(\\d+)/(\\d+)/(\\d+)\\s*\\"+mark._2).r
    str.trim match {
      case re_date(x1,x2,x3,y1,y2,y3) => 
        import java.util.Calendar
        val c1 = Calendar.getInstance()
        c1.clear
        c1.set(Integer.parseInt(x1),
               Integer.parseInt(x2)-1,
               Integer.parseInt(x3),0,0,0)
        val c2 = Calendar.getInstance()
        c2.clear
        c2.set(Integer.parseInt(y1),
               Integer.parseInt(y2)-1,
               Integer.parseInt(y3),0,0,0)
        f(c1: Point, c2: Point)
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

  /**
   * The following intervals are not allowed,
   *   (+inf,8] [+inf,8) [3,-inf) [3,+inf]
   */  
  if((lowerPoint==pInfinite && upperPoint!=pInfinite) ||
     (lowerPoint==mInfinite && isLeftInclusive) ||
     (upperPoint==mInfinite && lowerPoint!=mInfinite) ||
     (upperPoint==pInfinite && isRightInclusive) ||
     (lowerPoint!=mInfinite && upperPoint!=pInfinite &&
      !lowerPoint.less(upperPoint)))
    throw new IntervalException(
        "%s should be lower than %s".format(lowerPoint, upperPoint)
    )

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

      val lp = max(lowerPoint, other.lowerPoint)
      val up = min(upperPoint, other.upperPoint)

      val left =
        if(!lowerPoint.greater(other.lowerPoint))
         other.isLeftInclusive
        else if(lowerPoint == other.lowerPoint)
          other.isLeftInclusive && isLeftInclusive
        else
          isLeftInclusive

      val right =
        if(!upperPoint.less(other.upperPoint))
          other.isRightInclusive
        else if(upperPoint == other.upperPoint)
          other.isRightInclusive && isRightInclusive
        else
          isRightInclusive

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
