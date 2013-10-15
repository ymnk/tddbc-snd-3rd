package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import java.io.{ByteArrayOutputStream => BAOS, ByteArrayInputStream => BAIS}
import java.io.{ObjectOutputStream => OOS, ObjectInputStream => OIS}
import java.io._

class ClosedIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "ClosedInterval"
  it should "be instantiated by lower and upper points" in {
    new ClosedInterval(3, 8) 
  }

  it should "impelement the pretty printer." in {
    new ClosedInterval(3, 8).toString should equal ("[3,8]")
  }

  it should "throw an exception if its arguments are wrong." in {
    intercept[IntervalException] {
      new ClosedInterval(8, 3) 
    }
  }

  it can "return its lower point." in {
    new ClosedInterval(3, 8).lowerPoint should equal (3)
  }

  it can "return its upper point." in {
    new ClosedInterval(3, 8).upperPoint should equal (8)
  }

  it should "support contains method." in {
    val interval = new ClosedInterval(3, 8) 
    interval.contains(3) should equal (true)
    interval.contains(8) should equal (true)
    interval.contains(-1) should equal (false)
    interval.contains(9) should equal (false)
  }

  it should "support equals method." in {
    val _3to8 = new ClosedInterval(3, 8)
    _3to8.equals(new ClosedInterval(3, 8)) should equal (true)
    _3to8.equals(new ClosedInterval(1, 6)) should equal (false)

    _3to8.equals(new OpenInterval(3, 8)) should equal (false)
    _3to8.equals(new OpenInterval(1, 6)) should equal (false)
  }

  it should "support isConnectedTo method." in {
    val _3to8 = new ClosedInterval(3, 8)

    _3to8.isConnectedTo(new ClosedInterval(1, 6)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(1, 9)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(6, 9)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(1, 3)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(8, 15)) should equal (true)
    _3to8.isConnectedTo(new ClosedInterval(9, 12)) should equal (false)
  }

  it should "support isConnectedTo method for other intervals." in {
    val _3to8 = new ClosedInterval(3, 8)

    val true_cases = Array(
      new ClosedOpenInterval(8, 15),
      new OpenClosedInterval(1, 3)
    )

    for(interval <- true_cases) {
      _3to8.isConnectedTo(interval) should equal (true)
    }

    val false_cases = Array(
      new ClosedOpenInterval(1, 3),
      new OpenClosedInterval(8, 15),
      new OpenInterval(1, 3), new OpenInterval(8, 15)
    )

    for(interval <- false_cases) {
      _3to8.isConnectedTo(interval) should equal (false)
    }
  }

  it should "support containsAll method." in {
    val _3to8 = new ClosedInterval(3, 8)

    _3to8.containsAll(Array(4, 7, 3)) should equal (true)
    _3to8.containsAll(Array(6, -1)) should equal (false)
  }

  it should "support getIntersection method." in {
    val c3to8 = new ClosedInterval(3, 8)
    val c4to10 = new ClosedInterval(4, 10)
    val c9to12 = new ClosedInterval(9, 12)

    c3to8.getIntersection(c4to10).toString should equal ("[4,8]")
    intercept[IntervalException] {
      c3to8.getIntersection(c9to12)
    }
  }

  it should "support parse method." in {
    ClosedInterval.parse("[3,8]").toString should equal ("[3,8]")
  }
}
