package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class InfiniteIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "InfiniteInterval"

  it should "be instantiated by lower and upper points" in {
    OpenInterval(mInfinite, pInfinite)
    OpenInterval(3, pInfinite) 
    OpenInterval(mInfinite, 8) 
    intercept[IntervalException] { ClosedInterval(3, pInfinite) }
    intercept[IntervalException] { ClosedInterval(mInfinite, 8) }
    intercept[IntervalException] { OpenClosedInterval(3, pInfinite) }
    intercept[IntervalException] { ClosedOpenInterval(mInfinite, 8) }
    OpenClosedInterval(mInfinite, 8)
    ClosedOpenInterval(3, pInfinite)
  }

  it should "impelement the pretty printer." in {
    OpenInterval(mInfinite, 8).toString should equal ("(-inf,8)")
    OpenInterval(3, pInfinite).toString should equal ("(3,+inf)")
    OpenInterval(mInfinite, pInfinite).toString should equal ("(-inf,+inf)")
  }

  it should "throw an exception if its arguments are wrong." in {
    intercept[IntervalException] {
      OpenInterval(pInfinite, mInfinite) 
    }
    intercept[IntervalException] {
      OpenInterval(pInfinite, 8)
    }
    intercept[IntervalException] {
      OpenInterval(3, mInfinite)
    }
  }

  it should "return its lower point." in {
    OpenInterval(mInfinite, 8).lowerPoint should equal (mInfinite)
  }

  it should "return its upper point." in {
    OpenInterval(3, pInfinite).upperPoint should equal (pInfinite)
  }

  it should "contains method." in {
    val _mito8 = OpenInterval(mInfinite, 8)
    _mito8.contains(0) should equal (true)
    _mito8.contains(8) should equal (false)
    _mito8.contains(10) should equal (false)
    val _3topi = OpenInterval(3, pInfinite)
    _3topi.contains(0) should equal (false)
    _3topi.contains(3) should equal (false)
    _3topi.contains(10) should equal (true)
  }

  it should "support equals method." in {
    val _3to8 = OpenInterval(3, 8)

    _3to8.equals(OpenInterval(3, 8)) should equal (true)
    _3to8.equals(OpenInterval(1, 6)) should equal (false)

    _3to8.equals(ClosedInterval(3, 8)) should equal (false)
    _3to8.equals(ClosedInterval(1, 6)) should equal (false)
  }

  it should "support isConnectedTo method." in {
    val _3topi = OpenInterval(3, pInfinite)
    _3topi.isConnectedTo(OpenInterval(1, 6)) should equal (true)
    _3topi.isConnectedTo(OpenInterval(1, 9)) should equal (true)
    _3topi.isConnectedTo(OpenInterval(6, 9)) should equal (true)
    _3topi.isConnectedTo(OpenInterval(1, 3)) should equal (false)
    _3topi.isConnectedTo(OpenInterval(8, 15)) should equal (true)

    val _mito8 = OpenInterval(mInfinite, 8)
    _mito8.isConnectedTo(OpenInterval(1, 8)) should equal (true)
    _mito8.isConnectedTo(OpenInterval(8, 15)) should equal (false)
    _mito8.isConnectedTo(OpenInterval(9, 15)) should equal (false)
  }

/*
  it should "support isConnectedTo method for other intervals." in {
    val _3to8 = OpenInterval(3, 8)

    val false_cases = Array(
      ClosedInterval(1, 3), ClosedInterval(8, 15),
      OpenInterval(1, 3), OpenInterval(8, 15),
      ClosedOpenInterval(1, 3), ClosedOpenInterval(8, 15),
      OpenClosedInterval(1, 3), OpenClosedInterval(8, 15))

    for(interval <- false_cases){
      _3to8.isConnectedTo(interval) should equal (false)
    }
  }
*/

  it should "support containsAll method." in {
    val _3topi = OpenInterval(3, pInfinite)
    _3topi.containsAll(Array(2, 3, 4, 7)) should equal (false)
    _3topi.containsAll(Array(3, 4, 7)) should equal (false)
    _3topi.containsAll(Array(4, 7)) should equal (true)
  }

  it should "support getIntersection method for other intervals." in {
    val _3topi = OpenInterval(3, pInfinite)
    import _3topi.{getIntersection => _3topi_gi}

    val intervals =
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
      for(interval <- intervals) {
        if(!_3topi.isConnectedTo(interval)){
          intercept[IntervalException] {
            _3topi_gi(interval)
          }
        }
        else if(_3topi.contains(i) && interval.contains(i)){
          _3topi_gi(interval).contains(i) should equal (true)
        }
        else {
          _3topi_gi(interval).contains(i) should equal (false)
        }
      }
    }

    val _mito8 = OpenInterval(mInfinite, 8)
    import _mito8.{getIntersection => _mito8_gi}

    for(i <- 0 to 15) {
      for(interval <- intervals) {
        if(!_mito8.isConnectedTo(interval)){
          intercept[IntervalException] {
            _mito8_gi(interval)
          }
        }
        else if(_mito8.contains(i) && interval.contains(i)){
          _mito8_gi(interval).contains(i) should equal (true)
        }
        else {
          _mito8_gi(interval).contains(i) should equal (false)
        }
      }
    }
  }

  it should "support parse method." in {
    OpenInterval.parse("(-inf,8)").toString should equal ("(-inf,8)")
    OpenInterval.parse("(3,+inf)").toString should equal ("(3,+inf)")
  }
}
