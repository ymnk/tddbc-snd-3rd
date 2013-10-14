package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class OpenIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  var interval: OpenInterval = _

  before {
    interval = new OpenInterval(3, 8) 
  }

  after {
  }

  behavior of "OpenInterval"
  it should "be instaciated by lower and upper points" in {
    val interval = new OpenInterval(3, 8) 
  }

  it should "impelement the pretty printer." in {
    interval.toString should equal ("[3,8]")
  }

  it should "throw an exception if its arguments are wrong." in {
    intercept[IntervalException] {
      new OpenInterval(8, 3) 
    }
  }

  it can "return its lower point." in {
    interval.lowerPoint should equal (3)
  }

  it can "return its upper point." in {
    interval.upperPoint should equal (8)
  }

  it can "contains method." in {
    interval.contains(4) should equal (true)
    interval.contains(3) should equal (false)
    interval.contains(-1) should equal (false)
  }

  it can "support equlas method." in {
    val _3to8 = new OpenInterval(3, 8)

    _3to8.equals(new OpenInterval(3, 8)) should equal (true)
    _3to8.equals(new OpenInterval(1, 6)) should equal (false)

    _3to8.equals(new ClosedInterval(3, 8)) should equal (false)
    _3to8.equals(new ClosedInterval(1, 6)) should equal (false)
  }

  it can "support isConnectedTo method." in {
    val _3to8 = new OpenInterval(3, 8)

    _3to8.isConnectedTo(new OpenInterval(1, 6)) should equal (true)
    _3to8.isConnectedTo(new OpenInterval(8, 15)) should equal (false)
    _3to8.isConnectedTo(new OpenInterval(9, 12)) should equal (false)

    _3to8.isConnectedTo(new ClosedInterval(1, 6)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(8, 15)) should equal (false)
    _3to8.isConnectedTo(new ClosedInterval(9, 12)) should equal (false)
  }
}
