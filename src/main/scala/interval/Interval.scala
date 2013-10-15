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

abstract class Interval {
  import Interval.isInclusive

  def lowerPoint: Int
  def upperPoint: Int

  def leftEnd: String
  def rightEnd: String

  if(lowerPoint > upperPoint)
    throw new IntervalException(
      "%d should be lower than %d".format(lowerPoint, upperPoint)
    );

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

  override def toString: String =
    (leftEnd+"%d,%d"+rightEnd).format(lowerPoint, upperPoint)
}
