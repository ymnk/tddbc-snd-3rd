package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import Point.pointType._
import java.util.Calendar

class CalendarIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  private def d(y: Int, m: Int, d:Int) = {
   val cal = Calendar.getInstance()
   cal.clear
   cal.set(y, m, d, 0, 0, 0)
   cal
  }
 
  behavior of "Calendar Interval"
  it should "be instantiated by lower and upper points" in {
    OpenInterval(d(2013,3,1), d(2013,8,1))
  }

  it should "impelement the pretty printer." in {
    OpenInterval(d(2013,3,1), d(2013,8,1)).toString should equal ("(2013/4/1,2013/9/1)")
  }

  it should "throw an exception if its arguments are wrong." in {
    intercept[IntervalException] {
      OpenInterval(d(2013,8,1), d(2013,3,1))
    }
  }

  it should "return its lower point." in {
    OpenInterval(d(2013,3,1), d(2013,8,1)).lowerPoint should equal (d(2013,3,1): Point)
  }

  it should "return its upper point." in {
    OpenInterval(d(2013,3,1), d(2013,8,1)).upperPoint should equal (d(2013,8,1): Point)
  }

  it should "contains method." in {
    val interval = OpenInterval(d(2013,3,1), d(2013,8,1))
    interval.contains(d(2013,4,1)) should equal (true)
    interval.contains(d(2013,3,1)) should equal (false)
    interval.contains(d(2013,2,1)) should equal (false)
  }

  it should "support parse method." in {
    OpenInterval.parse("(2013/4/1,2013/9/1)").toString should equal ("(2013/4/1,2013/9/1)")
  }
}
