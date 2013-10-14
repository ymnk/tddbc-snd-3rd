package interval

class OpenInterval(val lowerPoint: Int, val upperPoint: Int) {

  if(lowerPoint > upperPoint)
    throw new IntervalException(
      "%d should be lower than %d".format(lowerPoint, upperPoint)
    );

  override def toString: String =
    "[%d,%d]".format(lowerPoint, upperPoint)
} 