package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class OpenIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "OpenInterval"
  it should "be instantiated by lower and upper points" in {
    new OpenInterval(3, 8) 
  }

  it should "impelement the pretty printer." in {
    new OpenInterval(3, 8).toString should equal ("(3,8)")
  }

  it should "throw an exception if its arguments are wrong." in {
    intercept[IntervalException] {
      new OpenInterval(8, 3) 
    }
  }

  it should "return its lower point." in {
    new OpenInterval(3, 8).lowerPoint should equal (3)
  }

  it should "return its upper point." in {
    new OpenInterval(3, 8).upperPoint should equal (8)
  }

  it should "contains method." in {
    val interval = new OpenInterval(3, 8)
    interval.contains(4) should equal (true)
    interval.contains(3) should equal (false)
    interval.contains(-1) should equal (false)
  }

  it should "support equals method." in {
    val _3to8 = new OpenInterval(3, 8)

    _3to8.equals(new OpenInterval(3, 8)) should equal (true)
    _3to8.equals(new OpenInterval(1, 6)) should equal (false)

    _3to8.equals(new ClosedInterval(3, 8)) should equal (false)
    _3to8.equals(new ClosedInterval(1, 6)) should equal (false)
  }

  it should "support isConnectedTo method." in {
    val _3to8 = new OpenInterval(3, 8)

    _3to8.isConnectedTo(new OpenInterval(1, 6)) should equal (true)
    _3to8.isConnectedTo(new OpenInterval(1, 9)) should equal (true)
    _3to8.isConnectedTo(new OpenInterval(6, 9)) should equal (true)
    _3to8.isConnectedTo(new OpenInterval(1, 3)) should equal (false)
    _3to8.isConnectedTo(new OpenInterval(8, 15)) should equal (false)
    _3to8.isConnectedTo(new OpenInterval(9, 12)) should equal (false)
  }

  it should "support isConnectedTo method for other intervals." in {
    val _3to8 = new OpenInterval(3, 8)

    val false_cases = Array(
      new ClosedInterval(1, 3), new ClosedInterval(8, 15),
      new OpenInterval(1, 3), new OpenInterval(8, 15),
      new ClosedOpenInterval(1, 3), new ClosedOpenInterval(8, 15),
      new OpenClosedInterval(1, 3), new OpenClosedInterval(8, 15))

    for(interval <- false_cases){
      _3to8.isConnectedTo(interval) should equal (false)
    }
   }

  it should "support containsAll method." in {
    val _3to8 = new OpenInterval(3, 8)

    _3to8.containsAll(Array(4, 7, 3)) should equal (false)
    _3to8.containsAll(Array(6, -1)) should equal (false)
  }

  it should "support parse method." in {
    OpenInterval.parse("(3,8)").toString should equal ("(3,8)")
  }
}
