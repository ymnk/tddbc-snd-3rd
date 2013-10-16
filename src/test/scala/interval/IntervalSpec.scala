package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import Point.pointType._

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

  it should "support getIntersection method." in {

    val intervals =
      OpenClosedInterval(mInfinite, 8) ::
      ClosedOpenInterval(3, pInfinite) ::
      OpenInterval(3, pInfinite) ::
      OpenInterval(mInfinite, 8) ::
      List(OpenInterval.apply _,
           ClosedInterval.apply _,
           OpenClosedInterval.apply _,
           ClosedOpenInterval.apply _).flatMap { f =>
        List((1,3), 
             (3,7), (3,8), (3,10),
             (4,7), (4,8), (4,10),
             (8,10)).map { p =>
        f(p._1, p._2)
      }
    }

    for(i <- 0 to 15) {
      for(ival1 <- intervals) {
        for(ival2 <- intervals) {
          if(!ival2.isConnectedTo(ival1)){
            intercept[IntervalException] {
              ival2.getIntersection(ival1)
            }
          }
          else if(ival2.contains(i) && ival1.contains(i)){
            ival2.getIntersection(ival1).contains(i) should equal (true)
          }
          else {
            ival2.getIntersection(ival1).contains(i) should equal (false)
          }
        } 
      }
    }
  }

  it should "implement a perser." in {
    Interval.parse("(3,8)") should equal (OpenInterval(3, 8))
    Interval.parse("[3,8]") should equal (ClosedInterval(3, 8))
    Interval.parse("(3,8]") should equal (OpenClosedInterval(3, 8))
    Interval.parse("[3,8)") should equal (ClosedOpenInterval(3, 8))
  }
}
