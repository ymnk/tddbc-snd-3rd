package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks._

import Point.pointType._

class OpenIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "OpenInterval"
  it should "be instantiated by lower and upper points" in {
    OpenInterval(3, 8) 
  }

  it should "impelement the pretty printer." in {
    OpenInterval(3, 8).toString should equal ("(3,8)")
  }

  it should "throw an exception if its arguments are wrong." in {
    OpenInterval(3, 3) 
    intercept[IntervalException] {
      OpenInterval(8, 3) 
    }
    OpenInterval(8, 8) 
  }

  it should "return its lower point." in {
    OpenInterval(3, 8).lowerPoint should equal (3: Point)
  }

  it should "return its upper point." in {
    OpenInterval(3, 8).upperPoint should equal (8: Point)
  }

  it should "contains method." in {
    val interval = OpenInterval(3, 8)

    forAll(
      Table(
        ("n", "v"),
        (4,   true),
        (3,   false),
        (-1,  false))){ (n, v) =>
      interval.contains(n) should equal (v)
    }
  }

  it should "support equals method." in {
    val _3to8 = OpenInterval(3, 8)

    forAll(
      Table(
        ("i",                   "v"),
        (OpenInterval(3, 8),    true),
        (OpenInterval(1, 6),    false),
        (ClosedInterval(3, 8),  false),
        (ClosedInterval(1, 6),  false))){ (i, v) =>
      _3to8.equals(i) should equal (v)
    }
  }

  it should "support isConnectedTo method." in {
    val _3to8 = OpenInterval(3, 8)

    forAll(
      Table(
        ("i",                   "v"),
        (OpenInterval(1, 6),  true),
        (OpenInterval(1, 9),  true),
        (OpenInterval(6, 9),  true),
        (OpenInterval(1, 3),  false),
        (OpenInterval(8, 15), false),
        (OpenInterval(9, 12), false))){ (i, v) =>
      _3to8.isConnectedTo(i) should equal (v)
    }
  }

  it should "support isConnectedTo method for other intervals." in {
    val _3to8 = OpenInterval(3, 8)

    forAll(
      Table(
        ("i",                        "v"),
        (ClosedInterval(1, 3),      false),
        (ClosedInterval(8, 15),     false),
        (OpenInterval(1, 3),        false),
        (OpenInterval(8, 15),       false),
        (ClosedOpenInterval(1, 3),  false),
        (ClosedOpenInterval(8, 15), false),
        (OpenClosedInterval(1, 3),  false),
        (OpenClosedInterval(8, 15), false))){ (i, v) =>
      _3to8.isConnectedTo(i) should equal (v)
    }
  }

  it should "support containsAll method." in {
    val _3to8 = OpenInterval(3, 8)

    forAll(
      Table(
        ("a",                        "v"),
        (Array(4, 7, 3),             false),
        (Array(6, -1),               false))){ (a, v) =>
      _3to8.containsAll(a) should equal (v)
    }
  }

  it should "support getIntersection method for other intervals." in {
    val _3to8 = OpenInterval(3, 8)

    forAll(
      Table("i",
            ClosedInterval(1, 3),
            OpenClosedInterval(1, 3),
            ClosedOpenInterval(1, 3),
            OpenInterval(1, 3))) { i =>
      intercept[IntervalException] {
        _3to8.getIntersection(i) //  "(3,3)"
      }
    }

    forAll(
      Table(
        ("i",                        "v"),
        (OpenInterval(3, 7),         "(3,7)"),
        (ClosedInterval(3, 7),       "(3,7]"),
        (OpenClosedInterval(3, 7),   "(3,7]"),
        (ClosedOpenInterval(3, 7),   "(3,7)"),

        (OpenInterval(3, 8),         "(3,8)"),
        (ClosedInterval(3, 8),       "(3,8)"),
        (OpenClosedInterval(3, 8),   "(3,8)"),
        (ClosedOpenInterval(3, 8),   "(3,8)"),

        (OpenInterval(3, 10),        "(3,8)"),
        (ClosedInterval(3, 10),      "(3,8)"),
        (OpenClosedInterval(3, 10),  "(3,8)"),
        (ClosedOpenInterval(3, 10),  "(3,8)"),

        (OpenInterval(4, 7),         "(4,7)"),
        (ClosedInterval(4, 7),       "[4,7]"),
        (OpenClosedInterval(4, 7),   "(4,7]"),
        (ClosedOpenInterval(4, 7),   "[4,7)"),

        (OpenInterval(4, 8),         "(4,8)"),
        (ClosedInterval(4, 8),       "[4,8)"),
        (OpenClosedInterval(4, 8),   "(4,8)"),
        (ClosedOpenInterval(4, 8),   "[4,8)"),

        (OpenInterval(4, 10),        "(4,8)"),
        (ClosedInterval(4, 10),      "[4,8)"),
        (OpenClosedInterval(4, 10),  "(4,8)"),
        (ClosedOpenInterval(4, 10),  "[4,8)"))){ (i, v) =>
      _3to8.getIntersection(i).toString should equal (v)
    }
  }

  it should "support parse method." in {
    OpenInterval.parse("(3,8)").toString should equal ("(3,8)")
  }
}
