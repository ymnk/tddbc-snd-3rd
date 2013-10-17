package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks._

import Point.pointType._

class IntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "Interval"

  it should "support filter method." in {
    val seq = (2 to 6).toList

    forAll(
      Table(
        ("p",                              "s"),
        (ClosedInterval(3, 5),             Set(3,4,5)),
        (OpenInterval(3, 5),               Set(4)),
        (ClosedOpenInterval(3, 5),         Set(3,4)),
        (ClosedOpenInterval(3, pInfinite), Set(3,4,5,6)),
        (OpenClosedInterval(3, 5),         Set(4,5)),
        (OpenClosedInterval(mInfinite, 5), Set(2,3,4,5)))){ (p, s) =>
      Interval.filter(seq, p) should equal (s)
    }
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

    forAll(
      Table(
        ("s",      "v"),
        ("(3,8)",  OpenInterval(3, 8)),
        ("[3,8]",  ClosedInterval(3, 8)),
        ("(3,8]",  OpenClosedInterval(3, 8)),
        ("[3,8)",  ClosedOpenInterval(3, 8)))){ (s, v) =>
      Interval.parse(s) should equal (v)
    }
  }
}
