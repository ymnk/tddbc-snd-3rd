package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class ClosedOpenIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "ClosedOpenInterval"
  it should "be instantiated by lower and upper points" in {
    new ClosedOpenInterval(3, 8) 
  }

  it should "impelement the pretty printer." in {
    new ClosedOpenInterval(3, 8).toString should equal ("[3,8)")
  }

  it should "throw an exception if its arguments are wrong." in {
    intercept[IntervalException] {
      new ClosedOpenInterval(8, 3) 
    }
  }

  it can "return its lower point." in {
    new ClosedOpenInterval(3, 8).lowerPoint should equal (3)
  }

  it can "return its upper point." in {
    new ClosedOpenInterval(3, 8).upperPoint should equal (8)
  }

  it should "support contains method." in {
    val interval = new ClosedOpenInterval(3, 8)
    interval.contains(4) should equal (true)
    interval.contains(3) should equal (true)
    interval.contains(8) should equal (false)
    interval.contains(-1) should equal (false)
    new ClosedOpenInterval(3, 3).contains(3) should equal (false)
  }

  it should "support equals method." in {
    val _3to8 = new ClosedOpenInterval(3, 8)

    _3to8.equals(new ClosedOpenInterval(3, 8)) should equal (true)
    _3to8.equals(new ClosedOpenInterval(1, 6)) should equal (false)

    _3to8.equals(new ClosedInterval(3, 8)) should equal (false)
    _3to8.equals(new ClosedInterval(1, 6)) should equal (false)
    _3to8.equals(new OpenInterval(3, 8)) should equal (false)
    _3to8.equals(new OpenInterval(1, 6)) should equal (false)
  }

  it should "support isConnectedTo method." in {
    val _3to8 = new ClosedOpenInterval(3, 8)

    _3to8.isConnectedTo(new ClosedOpenInterval(1, 6)) should equal (true)
    _3to8.isConnectedTo(new ClosedOpenInterval(1, 9)) should equal (true)
    _3to8.isConnectedTo(new ClosedOpenInterval(6, 9)) should equal (true)
    _3to8.isConnectedTo(new ClosedOpenInterval(1, 3)) should equal (false)
    _3to8.isConnectedTo(new ClosedOpenInterval(8, 15)) should equal (false)
    _3to8.isConnectedTo(new ClosedOpenInterval(9, 12)) should equal (false)
  }

  it should "support isConnectedTo method for other intervals." in {
    val _3to8 = new ClosedOpenInterval(3, 8)

    val true_cases = Array(
      new ClosedInterval(1, 3),
      new OpenClosedInterval(1, 3)
    )

    for(interval <- true_cases) {
      _3to8.isConnectedTo(interval) should equal (true)
    }

    val false_cases = Array(
      new ClosedInterval(8, 15),
      new OpenInterval(1, 3), new OpenInterval(8, 15),
      new OpenClosedInterval(8, 15)
    )

    for(interval <- false_cases) {
      _3to8.isConnectedTo(interval) should equal (false)
    }

    _3to8.isConnectedTo(new ClosedInterval(1, 6)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(1, 9)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(6, 9)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(8, 15)) should equal (false)
    _3to8.isConnectedTo(new ClosedInterval(9, 12)) should equal (false)
  }

  it should "support containsAll method." in {
    val _3to8 = new ClosedOpenInterval(3, 8)

    _3to8.containsAll(Array(4, 7, 3)) should equal (true)
    _3to8.containsAll(Array(4, 7, 3, 8)) should equal (false)
    _3to8.containsAll(Array(6, -1)) should equal (false)
  }

  it should "support getIntersection method for other intervals." in {
    val _3to8 = ClosedOpenInterval(3, 8)
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
  }

  it should "support parse method." in {
    ClosedOpenInterval.parse("[3,8)").toString should equal ("[3,8)")
  }
}
