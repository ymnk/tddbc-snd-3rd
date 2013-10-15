package interval

object Point {
  implicit def fromInt(p: Int): Point = new Point(p)
  implicit def fromStr(p: String): Point = p match {
    case "-inf" => mInfinite
    case "+inf" => pInfinite
    case _ =>
      throw new IntervalException("There is no way to change "+p+" to Point")
  }
}

class Point(_point: Int) {
  def point() = _point
  override def toString() = point.toString
  override def equals(other: Any) = other match {
    case that: Point if(!(that eq mInfinite) && !(that eq pInfinite)) =>
      point == that.point
    case _ => false
  }
}
object mInfinite extends Point(0) {
  override def point() = throw new IntervalException("-inf")
  override def toString() = "-inf"
  override def equals(other: Any) = other match {
    case that: Point if(that eq mInfinite) => true
    case _ => false
  }
}
object pInfinite extends Point(0) {
  override def point() = throw new IntervalException("+inf")
  override def toString() = "+inf"
  override def equals(other: Any) = other match {
    case that: Point if(that eq pInfinite) => true
    case _ => false
  }
}
