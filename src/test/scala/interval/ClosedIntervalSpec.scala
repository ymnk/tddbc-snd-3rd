package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks._

import Point.pointType._

class ClosedIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "ClosedInterval"
  it should "be instantiated by lower and upper points" in {
    ClosedInterval(3, 8) 
  }

  it should "impelement the pretty printer." in {
    ClosedInterval(3, 8).toString should equal ("[3,8]")
  }

  it should "throw an exception if its arguments are wrong." in {
    ClosedInterval(3, 3) 
    intercept[IntervalException] {
      ClosedInterval(8, 3) 
    }
    ClosedInterval(8, 8) 
  }

  it can "return its lower point." in {
    ClosedInterval(3, 8).lowerPoint should equal (3: Point)
  }

  it can "return its upper point." in {
    ClosedInterval(3, 8).upperPoint should equal (8: Point)
  }

  it should "support contains method." in {
    val interval = ClosedInterval(3, 8) 

    forAll(
      Table(
        ("n", "v"),
        (3,   true),
        (4,   true),
        (7,   true),
        (8,   true),
        (-1,  false),
        (2,   false),
        (9,   false))){ (n, v) =>
      interval.contains(n) should equal (v)
    }
  }

  it should "support equals method." in {
    val _3to8 = ClosedInterval(3, 8)

    forAll(
      Table(
        ("i",                   "v"),
        (ClosedInterval(3, 8),  true),
        (ClosedInterval(1, 6),  false),
        (OpenInterval(3, 8),    false),
        (OpenInterval(1, 6),    false))){ (i, v) =>
      _3to8.equals(i) should equal (v)
    }
  }

  it should "support isConnectedTo method." in {
    val _3to8 = ClosedInterval(3, 8)

    forAll(
      Table(
        ("i",                   "v"),
        (ClosedInterval(1, 2),  false),
        (ClosedInterval(1, 3),  true),
        (ClosedInterval(1, 6),  true),
        (ClosedInterval(1, 8),  true),
        (ClosedInterval(1, 9),  true),
        (ClosedInterval(6, 9),  true),
        (ClosedInterval(8, 15), true),
        (ClosedInterval(9, 12), false))){ (i, v) =>
      _3to8.isConnectedTo(i) should equal (v)
    }
  }

  it should "support isConnectedTo method for other intervals." in {
    val _3to8 = ClosedInterval(3, 8)

    forAll(
      Table(
        ("i",                        "v"),
        (ClosedOpenInterval(8, 15),  true),
        (OpenClosedInterval(1, 3),   true),
        (ClosedOpenInterval(1, 3),   false),
        (OpenClosedInterval(8, 15),  false),
        (OpenInterval(1, 3),         false),
        (OpenInterval(8, 15),        false))){ (i, v) =>
      _3to8.isConnectedTo(i) should equal (v)
    }
  }

  it should "support containsAll method." in {
    val _3to8 = ClosedInterval(3, 8)

    forAll(
      Table(
        ("a",                        "v"),
        (Array(4, 7, 3),             true),
        (Array(6, -1),               false))){ (a, v) =>
      _3to8.containsAll(a) should equal (v)
    }
  }

  it should "support getIntersection method." in {
    val c3to8 = ClosedInterval(3, 8)
    val c4to10 = ClosedInterval(4, 10)
    val c9to12 = ClosedInterval(9, 12)

    c3to8.getIntersection(c4to10).toString should equal ("[4,8]")
    intercept[IntervalException] {
      c3to8.getIntersection(c9to12)
    }
  }

  it should "support getIntersection method for other intervals." in {
    val _3to8 = ClosedInterval(3, 8)

    forAll(
      Table("i",
            OpenInterval(1, 3),
            ClosedOpenInterval(1, 3))) { i =>
      intercept[IntervalException] {
        _3to8.getIntersection(i) //  "(3,3)"
      }
    }

    forAll(
      Table(
        ("i",                        "v"),
        (ClosedInterval(1, 3),       "[3,3]"),
        (OpenClosedInterval(1, 3),   "[3,3]"),

        (OpenInterval(3, 7),         "(3,7)"),
        (ClosedInterval(3, 7),       "[3,7]"),
        (OpenClosedInterval(3, 7),   "(3,7]"),
        (ClosedOpenInterval(3, 7),   "[3,7)"),

        (OpenInterval(3, 8),         "(3,8)"),
        (ClosedInterval(3, 8),       "[3,8]"),
        (OpenClosedInterval(3, 8),   "(3,8]"),
        (ClosedOpenInterval(3, 8),   "[3,8)"),

        (OpenInterval(3, 10),        "(3,8]"),
        (ClosedInterval(3, 10),      "[3,8]"),
        (OpenClosedInterval(3, 10),  "(3,8]"),
        (ClosedOpenInterval(3, 10),  "[3,8]"),

        (OpenInterval(4, 7),         "(4,7)"),
        (ClosedInterval(4, 7),       "[4,7]"),
        (OpenClosedInterval(4, 7),   "(4,7]"),
        (ClosedOpenInterval(4, 7),   "[4,7)"),

        (OpenInterval(4, 8),         "(4,8)"),
        (ClosedInterval(4, 8),       "[4,8]"),
        (OpenClosedInterval(4, 8),   "(4,8]"),
        (ClosedOpenInterval(4, 8),   "[4,8)"),

        (OpenInterval(4, 10),        "(4,8]"),
        (ClosedInterval(4, 10),      "[4,8]"),
        (OpenClosedInterval(4, 10),  "(4,8]"),
        (ClosedOpenInterval(4, 10),  "[4,8]"))){ (i, v) =>
      _3to8.getIntersection(i).toString should equal (v)
    }
  }

  it should "support parse method." in {
    ClosedInterval.parse("[3,8]").toString should equal ("[3,8]")
    ClosedInterval.parse("[+3,+8]").toString should equal ("[3,8]")
    ClosedInterval.parse("[-8,-3]").toString should equal ("[-8,-3]")
  }
}
