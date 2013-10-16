package interval

trait PointType[T] {
  def convSeq(seq: Seq[T]): Seq[Point]
  def unconvSet(set: Set[Point]): Set[T]
}
