package interval

abstract class Interval(val lowerPoint: Int, val upperPoint: Int) {
  if(lowerPoint > upperPoint)
    throw new IntervalException(
      "%d should be lower than %d".format(lowerPoint, upperPoint)
    );

  def contains(arg: Int): Boolean

  def containsAll(arg: Seq[Int]) = arg.forall(this.contains(_))
}
