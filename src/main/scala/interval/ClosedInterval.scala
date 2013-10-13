package interval

class ClosedInterval(val lowerPoint: Int, val upperPoint: Int) {
  override def toString: String =
    "[%d,%d]".format(lowerPoint, upperPoint)
} 
