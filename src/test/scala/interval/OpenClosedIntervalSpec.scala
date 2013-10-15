package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class OpenClosedIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "OpenClosedInterval"
  it should "be instantiated by lower and upper points" in {
    new OpenClosedInterval(3, 8) 
  }

  it should "impelement the pretty printer." in {
    new OpenClosedInterval(3, 8).toString should equal ("(3,8]")
  }

  it should "throw an exception if its arguments are wrong." in {
    intercept[IntervalException] {
      new OpenClosedInterval(8, 3) 
    }
  }

  it can "return its lower point." in {
    new OpenClosedInterval(3, 8).lowerPoint should equal (3)
  }

  it can "return its upper point." in {
    new OpenClosedInterval(3, 8).upperPoint should equal (8)
  }

  it should "support contains method." in {
    val interval = new OpenClosedInterval(3, 8)
    interval.contains(4) should equal (true)
    interval.contains(3) should equal (false)
    interval.contains(8) should equal (true)
    interval.contains(-1) should equal (false)
    new OpenClosedInterval(3, 3).contains(3) should equal (false)
  }

  it should "support equals method." in {
    val _3to8 = new OpenClosedInterval(3, 8)

    _3to8.equals(new OpenClosedInterval(3, 8)) should equal (true)
    _3to8.equals(new OpenClosedInterval(1, 6)) should equal (false)

    _3to8.equals(new ClosedInterval(3, 8)) should equal (false)
    _3to8.equals(new ClosedInterval(1, 6)) should equal (false)
    _3to8.equals(new OpenInterval(3, 8)) should equal (false)
    _3to8.equals(new OpenInterval(1, 6)) should equal (false)
  }

  it should "support isConnectedTo method." in {
    val _3to8 = new OpenClosedInterval(3, 8)

    _3to8.isConnectedTo(new OpenClosedInterval(1, 6)) should equal (true)
    _3to8.isConnectedTo(new OpenClosedInterval(1, 9)) should equal (true)
    _3to8.isConnectedTo(new OpenClosedInterval(6, 9)) should equal (true)
    _3to8.isConnectedTo(new OpenClosedInterval(1, 3)) should equal (false)
    _3to8.isConnectedTo(new OpenClosedInterval(8, 15)) should equal (false)
    _3to8.isConnectedTo(new OpenClosedInterval(9, 12)) should equal (false)
  }

  it should "support isConnectedTo method for other intervals." in {
    val _3to8 = new OpenClosedInterval(3, 8)

    val true_cases = Array(
      new ClosedInterval(8, 15),
      new ClosedOpenInterval(8, 15)
    )

    for(interval <- true_cases) {
      _3to8.isConnectedTo(interval) should equal (true)
    }

    val false_cases = Array(
      new ClosedInterval(1, 3),
      new OpenInterval(1, 3), new OpenInterval(8, 15),
      new ClosedOpenInterval(1, 3)
    )

    for(interval <- false_cases) {
      _3to8.isConnectedTo(interval) should equal (false)
    }
  }

  it should "support containsAll method." in {
    val _3to8 = new OpenClosedInterval(3, 8)

    _3to8.containsAll(Array(4, 7, 8)) should equal (true)
    _3to8.containsAll(Array(4, 7, 3, 8)) should equal (false)
    _3to8.containsAll(Array(6, -1)) should equal (false)
  }

  it should "support getIntersection method for other intervals." in {
    val _3to8 = OpenClosedInterval(3, 8)
    import _3to8.{getIntersection => _3to8_gi}

    val intervals =
      List(OpenInterval.apply _, ClosedInterval.apply _,
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
      for(interval <- intervals) {
        if(!_3to8.isConnectedTo(interval)){
          intercept[IntervalException] {
            _3to8_gi(interval)
          }
        }
        else if(_3to8.contains(i) && interval.contains(i)){
          _3to8_gi(interval).contains(i) should equal (true)
        }
        else {
          _3to8_gi(interval).contains(i) should equal (false)
        }
      }
    }

    intercept[IntervalException] {
      _3to8_gi(ClosedInterval(1, 3)) // "(3,3)"
    }

    intercept[IntervalException] {
      _3to8_gi(OpenClosedInterval(1, 3)) // "(3,3)"
    }

    intercept[IntervalException] {
      _3to8_gi(ClosedOpenInterval(1, 3)) //  "(3,3)"
    }

    intercept[IntervalException] {
      _3to8_gi(OpenInterval(1, 3)) //  "(3,3)"
    }

    _3to8_gi(OpenInterval(3, 7)).toString should equal ("(3,7)")
    _3to8_gi(ClosedInterval(3, 7)).toString should equal ("(3,7]")
    _3to8_gi(OpenClosedInterval(3, 7)).toString should equal ("(3,7]")
    _3to8_gi(ClosedOpenInterval(3, 7)).toString should equal ("(3,7)")

    _3to8_gi(OpenInterval(3, 8)).toString should equal ("(3,8)")
    _3to8_gi(ClosedInterval(3, 8)).toString should equal ("(3,8]")
    _3to8_gi(OpenClosedInterval(3, 8)).toString should equal ("(3,8]")
    _3to8_gi(ClosedOpenInterval(3, 8)).toString should equal ("(3,8)")

    _3to8_gi(OpenInterval(3, 10)).toString should equal ("(3,8]")
    _3to8_gi(ClosedInterval(3, 10)).toString should equal ("(3,8]")
    _3to8_gi(OpenClosedInterval(3, 10)).toString should equal ("(3,8]")
    _3to8_gi(ClosedOpenInterval(3, 10)).toString should equal ("(3,8]")

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

  it can "support parse method." in {
    OpenClosedInterval.parse("(3,8]").toString should equal ("(3,8]")
  }
}
