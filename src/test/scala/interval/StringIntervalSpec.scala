package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import Point.pointType._

class StringIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "String Interval"
  it should "be instantiated by lower and upper points" in {
    OpenInterval("B", "D") 
  }

  it should "impelement the pretty printer." in {
    OpenInterval("B", "D").toString should equal ("(B,D)")
  }


  it should "throw an exception if its arguments are wrong." in {
    intercept[IntervalException] {
      OpenInterval("D", "B") 
    }
  }


  it should "return its lower point." in {
    OpenInterval("B", "D").lowerPoint should equal ("B": Point)
  }

  it should "return its upper point." in {
    OpenInterval("B", "D").upperPoint should equal ("D": Point)
  }


  it should "contains method." in {
    val interval = OpenInterval("B", "D")
    interval.contains("C") should equal (true)
    interval.contains("B") should equal (false)
    interval.contains("A") should equal (false)
  }

  it should "support equals method." in {
    val _btod = OpenInterval("B", "D")

    _btod.equals(OpenInterval("B", "D")) should equal (true)
    _btod.equals(OpenInterval("A", "C")) should equal (false)

    _btod.equals(ClosedInterval("B", "D")) should equal (false)
    _btod.equals(ClosedInterval("A", "C")) should equal (false)
  }

  it should "support isConnectedTo method." in {
    val _btod = OpenInterval("B", "D")
    _btod.isConnectedTo(OpenInterval("A", "C")) should equal (true)
    _btod.isConnectedTo(OpenInterval("A", "E")) should equal (true)
    _btod.isConnectedTo(OpenInterval("C", "E")) should equal (true)
    _btod.isConnectedTo(OpenInterval("A", "B")) should equal (false)
    _btod.isConnectedTo(OpenInterval("D", "F")) should equal (false)
    _btod.isConnectedTo(OpenInterval("E", "F")) should equal (false)
  }

  it should "support isConnectedTo method for other intervals." in {
    val _btod = OpenInterval("B", "D")

    val false_cases = Array(
      ClosedInterval("A", "B"), ClosedInterval("D", "F"),
      OpenInterval("A", "B"), OpenInterval("D", "F"),
      ClosedOpenInterval("A", "B"), ClosedOpenInterval("D", "F"),
      OpenClosedInterval("A", "B"), OpenClosedInterval("D", "F")
    )

    for(interval <- false_cases){
      _btod.isConnectedTo(interval) should equal (false)
    }
  }

  it should "support containsAll method." in {
    val _btod = OpenInterval("B", "D")
    _btod.containsAll(Array("C", "E", "B")) should equal (false)
    _btod.containsAll(Array("C", "A")) should equal (false)
    _btod.containsAll(Array("C")) should equal (true)
  }

  it should "support getIntersection method for other intervals." in {
    val _btoe = OpenInterval("B", "E")
    import _btoe.{getIntersection => _btoe_gi}

    intercept[IntervalException] {
      _btoe_gi(ClosedInterval("A", "B"))
    }

    intercept[IntervalException] {
      _btoe_gi(OpenClosedInterval("A", "B"))
    }

    intercept[IntervalException] {
      _btoe_gi(ClosedOpenInterval("A", "B"))
    }

    intercept[IntervalException] {
      _btoe_gi(OpenInterval("A", "B"))
    }

    _btoe_gi(OpenInterval("B", "C")).toString should equal ("(B,C)")
    _btoe_gi(ClosedInterval("B", "C")).toString should equal ("(B,C]")
    _btoe_gi(OpenClosedInterval("B", "C")).toString should equal ("(B,C]")
    _btoe_gi(ClosedOpenInterval("B", "C")).toString should equal ("(B,C)")

    _btoe_gi(OpenInterval("B", "E")).toString should equal ("(B,E)")
    _btoe_gi(ClosedInterval("B", "E")).toString should equal ("(B,E)")
    _btoe_gi(OpenClosedInterval("B", "E")).toString should equal ("(B,E)")
    _btoe_gi(ClosedOpenInterval("B", "E")).toString should equal ("(B,E)")

    _btoe_gi(OpenInterval("B", "F")).toString should equal ("(B,E)")
    _btoe_gi(ClosedInterval("B", "F")).toString should equal ("(B,E)")
    _btoe_gi(OpenClosedInterval("B", "F")).toString should equal ("(B,E)")
    _btoe_gi(ClosedOpenInterval("B", "F")).toString should equal ("(B,E)")

    _btoe_gi(OpenInterval("C", "D")).toString should equal ("(C,D)")
    _btoe_gi(ClosedInterval("C", "D")).toString should equal ("[C,D]")
    _btoe_gi(OpenClosedInterval("C", "D")).toString should equal ("(C,D]")
    _btoe_gi(ClosedOpenInterval("C", "D")).toString should equal ("[C,D)")

    _btoe_gi(OpenInterval("C", "E")).toString should equal ("(C,E)")
    _btoe_gi(ClosedInterval("C", "E")).toString should equal ("[C,E)")
    _btoe_gi(OpenClosedInterval("C", "E")).toString should equal ("(C,E)")
    _btoe_gi(ClosedOpenInterval("C", "E")).toString should equal ("[C,E)")

    _btoe_gi(OpenInterval("C", "F")).toString should equal ("(C,E)")
    _btoe_gi(ClosedInterval("C", "F")).toString should equal ("[C,E)")
    _btoe_gi(OpenClosedInterval("C", "F")).toString should equal ("(C,E)")
    _btoe_gi(ClosedOpenInterval("C", "F")).toString should equal ("[C,E)")

    val intervals =
      OpenClosedInterval(mInfinite, "E") ::
      ClosedOpenInterval("B", pInfinite) ::
      OpenInterval("B", pInfinite) ::
      OpenInterval(mInfinite, "E") ::
      List(OpenInterval.apply _,
           ClosedInterval.apply _,
           OpenClosedInterval.apply _,
           ClosedOpenInterval.apply _).flatMap { f =>
        List(("A","B"), 
             ("B","D"), ("B","E"), ("B","F"),
             ("C","D"), ("C","E"), ("C","F"),
             ("E","F")).map { p =>
        f(p._1, p._2)
      }
    }

    for(i <- Array("A","B","C","D","E","F","G")) {
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

  it should "support parse method." in {
    OpenInterval.parse("(B,D)").toString should equal ("(B,D)")
  }
}
