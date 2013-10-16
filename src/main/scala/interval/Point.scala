package interval

object Point {
  implicit def fromInt(p: Int): Point = new PointInt(p)
  implicit def fromStr(p: String): Point = p match {
    case "-inf" => mInfinite
    case "+inf" => pInfinite
    case _ => new PointString(p)
  }

  object pointType {
    implicit object PointTypeInt extends PointType[Int] {
      def convSeq(seq: Seq[Int]): Seq[Point] = seq.map(new PointInt(_))
      def unconvSet(set: Set[Point]): Set[Int] = set.map(_.asInstanceOf[PointInt].point)
    }
    implicit object PointTypePoint extends PointType[Point] {
     def convSeq(seq: Seq[Point]) = seq
     def unconvSet(seq: Set[Point]) = seq
    }
    implicit object PointTypeString extends PointType[String] {
     def convSeq(seq: Seq[String]): Seq[Point] = seq.map(new PointString(_))
     def unconvSet(set: Set[Point]): Set[String] = set.map(_.asInstanceOf[PointString].point)
    }
  }
}

trait Point {

  override def equals(other: Any) = other match {
    case _: pInfinite.type => false
    case _: mInfinite.type => false
    case _ =>
      throw new IntervalException(
        "equality is not defined between %s and %s".format(this, other)
      )
  }

  def less(other: Point): Boolean = other match {
    case _: pInfinite.type => true
    case _: mInfinite.type => false
    case _ => 
      throw new IntervalException(
        "less is not defined between %s and %s".format(this, other)
      )
  }

  def greater(other: Point): Boolean = other match {
    case _: pInfinite.type => false
    case _: mInfinite.type => true
    case _ => 
      throw new IntervalException(
        "greater is not defined between %s and %s".format(this, other)
      )
  }
}

object mInfinite extends Point {
  override def toString() = "-inf"
  override def equals(other: Any) = other match {
    case that: Point if(that eq mInfinite) => true
    case _ => false
  }
  override def less(other: Point) = true
  override def greater(other: Point) = other == mInfinite
}

object pInfinite extends Point {
  override def toString() = "+inf"
  override def equals(other: Any) = other match {
    case that: Point if(that eq pInfinite) => true
    case _ => false
  }
  override def less(other: Point) = other == pInfinite
  override def greater(other: Point) = true
}
