package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import Point.pointType._

class ClosedIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "ClosedInterval"
  it should "be instantiated by lower and upper points" in {
    ClosedInterval(3, 8) 
  }

  it should "impelement the pretty printer." in {
    ClosedInterval(3, 8).toString should equal ("[3,8]")
  }

  it should "throw an exception if its arguments are wrong." in {
    intercept[IntervalException] {
      ClosedInterval(8, 3) 
    }
  }

  it can "return its lower point." in {
    ClosedInterval(3, 8).lowerPoint should equal (3: Point)
  }

  it can "return its upper point." in {
    ClosedInterval(3, 8).upperPoint should equal (8: Point)
  }

  it should "support contains method." in {
    val interval = ClosedInterval(3, 8) 
    interval.contains(3) should equal (true)
    interval.contains(8) should equal (true)
    interval.contains(-1) should equal (false)
    interval.contains(9) should equal (false)
  }

  it should "support equals method." in {
    val _3to8 = ClosedInterval(3, 8)
    _3to8.equals(ClosedInterval(3, 8)) should equal (true)
    _3to8.equals(ClosedInterval(1, 6)) should equal (false)

    _3to8.equals(OpenInterval(3, 8)) should equal (false)
    _3to8.equals(OpenInterval(1, 6)) should equal (false)
  }

  it should "support isConnectedTo method." in {
    val _3to8 = ClosedInterval(3, 8)

    _3to8.isConnectedTo(ClosedInterval(1, 6)) should equal (true)
    _3to8.isConnectedTo(ClosedInterval(1, 9)) should equal (true)
    _3to8.isConnectedTo(ClosedInterval(6, 9)) should equal (true)
    _3to8.isConnectedTo(ClosedInterval(1, 3)) should equal (true)
    _3to8.isConnectedTo(ClosedInterval(8, 15)) should equal (true)
    _3to8.isConnectedTo(ClosedInterval(9, 12)) should equal (false)
  }

  it should "support isConnectedTo method for other intervals." in {
    val _3to8 = ClosedInterval(3, 8)

    val true_cases = Array(
      ClosedOpenInterval(8, 15),
      OpenClosedInterval(1, 3)
    )

    for(interval <- true_cases) {
      _3to8.isConnectedTo(interval) should equal (true)
    }

    val false_cases = Array(
      ClosedOpenInterval(1, 3),
      OpenClosedInterval(8, 15),
      OpenInterval(1, 3), OpenInterval(8, 15)
    )

    for(interval <- false_cases) {
      _3to8.isConnectedTo(interval) should equal (false)
    }
  }

  it should "support containsAll method." in {
    val _3to8 = ClosedInterval(3, 8)

    _3to8.containsAll(Array(4, 7, 3)) should equal (true)
    _3to8.containsAll(Array(6, -1)) should equal (false)
  }

  it should "support getIntersection method." in {
    val c3to8 = ClosedInterval(3, 8)
    val c4to10 = ClosedInterval(4, 10)
    val c9to12 = ClosedInterval(9, 12)

    c3to8.getIntersection(c4to10).toString should equal ("[4,8]")
    intercept[IntervalException] {
      c3to8.getIntersection(c9to12)
    }
  }

  it should "support getIntersection method for other intervals." in {
    val _3to8 = ClosedInterval(3, 8)
    import _3to8.{getIntersection => _3to8_gi}

    intercept[IntervalException] {
      _3to8_gi(OpenInterval(1, 3)) // "(3,3)"
    }
    _3to8_gi(ClosedInterval(1, 3)).toString should equal ("[3,3]")
    _3to8_gi(OpenClosedInterval(1, 3)).toString should equal ("[3,3]")
    intercept[IntervalException] {
      _3to8_gi(ClosedOpenInterval(1, 3)) //  "(3,3)"
    }

    _3to8_gi(OpenInterval(3, 7)).toString should equal ("(3,7)")
    _3to8_gi(ClosedInterval(3, 7)).toString should equal ("[3,7]")
    _3to8_gi(OpenClosedInterval(3, 7)).toString should equal ("(3,7]")
    _3to8_gi(ClosedOpenInterval(3, 7)).toString should equal ("[3,7)")

    _3to8_gi(OpenInterval(3, 8)).toString should equal ("(3,8)")
    _3to8_gi(ClosedInterval(3, 8)).toString should equal ("[3,8]")
    _3to8_gi(OpenClosedInterval(3, 8)).toString should equal ("(3,8]")
    _3to8_gi(ClosedOpenInterval(3, 8)).toString should equal ("[3,8)")

    _3to8_gi(OpenInterval(3, 10)).toString should equal ("(3,8]")
    _3to8_gi(ClosedInterval(3, 10)).toString should equal ("[3,8]")
    _3to8_gi(OpenClosedInterval(3, 10)).toString should equal ("(3,8]")
    _3to8_gi(ClosedOpenInterval(3, 10)).toString should equal ("[3,8]")

    _3to8_gi(OpenInterval(4, 7)).toString should equal ("(4,7)")
    _3to8_gi(ClosedInterval(4, 7)).toString should equal ("[4,7]")
    _3to8_gi(OpenClosedInterval(4, 7)).toString should equal ("(4,7]")
    _3to8_gi(ClosedOpenInterval(4, 7)).toString should equal ("[4,7)")

    _3to8_gi(OpenInterval(4, 8)).toString should equal ("(4,8)")
    _3to8_gi(ClosedInterval(4, 8)).toString should equal ("[4,8]")
    _3to8_gi(OpenClosedInterval(4, 8)).toString should equal ("(4,8]")
    _3to8_gi(ClosedOpenInterval(4, 8)).toString should equal ("[4,8)")

    _3to8_gi(OpenInterval(4, 10)).toString should equal ("(4,8]")
    _3to8_gi(ClosedInterval(4, 10)).toString should equal ("[4,8]")
    _3to8_gi(OpenClosedInterval(4, 10)).toString should equal ("(4,8]")
    _3to8_gi(ClosedOpenInterval(4, 10)).toString should equal ("[4,8]")
  }

  it should "support parse method." in {
    ClosedInterval.parse("[3,8]").toString should equal ("[3,8]")
  }
}
