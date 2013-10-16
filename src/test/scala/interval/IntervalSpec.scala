package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import java.io.{ByteArrayOutputStream => BAOS, ByteArrayInputStream => BAIS}
import java.io.{ObjectOutputStream => OOS, ObjectInputStream => OIS}
import java.io._

class IntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "Interval"

  it should "support filter method." in {
    val seq = (2 to 6).toList
    Interval.filter(seq, ClosedInterval(3, 5)) should equal (Set(3,4,5))
    Interval.filter(seq, OpenInterval(3, 5)) should equal (Set(4))
    Interval.filter(seq, ClosedOpenInterval(3, 5)) should equal (Set(3,4))
    Interval.filter(seq, ClosedOpenInterval(3, pInfinite)) should equal (Set(3,4,5,6))
    Interval.filter(seq, OpenClosedInterval(3, 5)) should equal (Set(4,5))
    Interval.filter(seq, OpenClosedInterval(mInfinite, 5)) should equal (Set(2,3,4,5))
  }
}
