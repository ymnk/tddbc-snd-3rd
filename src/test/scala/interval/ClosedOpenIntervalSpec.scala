package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class ClosedOpenIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "ClosedOpenInterval"
  it should "be instaciated by lower and upper points" in {
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

  it can "contains method." in {
    val interval = new ClosedOpenInterval(3, 8)
    interval.contains(4) should equal (true)
    interval.contains(3) should equal (true)
    interval.contains(8) should equal (false)
    interval.contains(-1) should equal (false)
  }

  it can "support equals method." in {
    val _3to8 = new ClosedOpenInterval(3, 8)

    _3to8.equals(new ClosedOpenInterval(3, 8)) should equal (true)
    _3to8.equals(new ClosedOpenInterval(1, 6)) should equal (false)

    _3to8.equals(new ClosedInterval(3, 8)) should equal (false)
    _3to8.equals(new ClosedInterval(1, 6)) should equal (false)
    _3to8.equals(new OpenInterval(3, 8)) should equal (false)
    _3to8.equals(new OpenInterval(1, 6)) should equal (false)
  }

  it can "support isConnectedTo method." in {
    val _3to8 = new ClosedOpenInterval(3, 8)

    _3to8.isConnectedTo(new ClosedOpenInterval(1, 6)) should equal (true)
    _3to8.isConnectedTo(new ClosedOpenInterval(1, 9)) should equal (true)
    _3to8.isConnectedTo(new ClosedOpenInterval(6, 9)) should equal (true)
    _3to8.isConnectedTo(new ClosedOpenInterval(1, 3)) should equal (false)
    _3to8.isConnectedTo(new ClosedOpenInterval(8, 15)) should equal (false)
    _3to8.isConnectedTo(new ClosedOpenInterval(9, 12)) should equal (false)
  }

  it can "support isConnectedTo method for ClosedInterval." in {
    val _3to8 = new ClosedOpenInterval(3, 8)

    _3to8.isConnectedTo(new ClosedInterval(1, 6)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(1, 9)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(6, 9)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(8, 15)) should equal (false)
    _3to8.isConnectedTo(new ClosedInterval(9, 12)) should equal (false)
  }

  it can "support containsAll method." in {
    val _3to8 = new ClosedOpenInterval(3, 8)

    _3to8.containsAll(Array(4, 7, 3)) should equal (true)
    _3to8.containsAll(Array(4, 7, 3, 8)) should equal (false)
    _3to8.containsAll(Array(6, -1)) should equal (false)
  }

  it can "support parse method." in {
    ClosedOpenInterval.parse("[3,8)").toString should equal ("[3,8)")
  }
}
