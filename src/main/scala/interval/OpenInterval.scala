package interval

object OpenInterval {
  val mark = ("(", ")")
  def parse(str: String): OpenInterval = Interval.parse(str, mark) match {
    case (x, y) => 
      OpenInterval(Integer.parseInt(x), Integer.parseInt(y))
  }
}

case class OpenInterval(val lowerPoint: Int, val upperPoint: Int) extends Interval {
  import OpenInterval.mark
  val leftEnd = mark._1
  val rightEnd = mark._2

  def contains(p: Int): Boolean = 
    (lowerPoint < p && p < upperPoint)

  def isConnectedTo(other: Interval) =
      !(upperPoint <= other.lowerPoint) && !(other.upperPoint <= lowerPoint)
} 
