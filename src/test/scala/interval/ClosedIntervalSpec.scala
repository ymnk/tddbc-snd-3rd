package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import java.io.{ByteArrayOutputStream => BAOS, ByteArrayInputStream => BAIS}
import java.io.{ObjectOutputStream => OOS, ObjectInputStream => OIS}
import java.io._

class Subject_1_1 extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "ClosedInterval"
  it should "be instaciated by lower and upper points" in {
    val interval = new ClosedInterval(3, 8) 
  }

  it should "impelement the pretty printer." in {
    val interval = new ClosedInterval(3, 8) 
    interval.toString should equal ("[3,8]")
  }

  it should "throw an exception if its arguments are wrong." in {
    intercept[IntervalException] {
      val interval = new ClosedInterval(8, 3) 
    }
  }

  it can "return its lower point." in {
    val interval = new ClosedInterval(3, 8) 
    interval.lowerPoint should equal (3)
  }

  it can "return its upper point." in {
    val interval = new ClosedInterval(3, 8) 
    interval.upperPoint should equal (8)
  }

  it can "contains method." in {
    val interval = new ClosedInterval(3, 8) 
    interval.contains(3) should equal (true)
    interval.contains(-1) should equal (false)
  }
}
