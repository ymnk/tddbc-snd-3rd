package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks._

import Point.pointType._

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

    forAll(
      Table("i",
            () => ClosedInterval(3, pInfinite),
            () => ClosedInterval(mInfinite, 8),
            () => OpenClosedInterval(3, pInfinite),
            () => ClosedOpenInterval(mInfinite, 8))) { i =>
      intercept[IntervalException] {
        i()
      }
    }

    OpenClosedInterval(mInfinite, 8)
    ClosedOpenInterval(3, pInfinite)
  }

  it should "impelement the pretty printer." in {

    forAll(
      Table(
        ("i",                                 "v"),
        (OpenInterval(mInfinite, 8),          "(-inf,8)"),
        (OpenInterval(3, pInfinite),          "(3,+inf)"),
        (OpenInterval(mInfinite, pInfinite),  "(-inf,+inf)"))){ (i, v) =>
      i.toString should equal (v)
    }
  }

  it should "throw an exception if its arguments are wrong." in {

    forAll(
      Table("i",
            () => OpenInterval(pInfinite, mInfinite),
            () => OpenInterval(pInfinite, 8),
            () => OpenInterval(3, mInfinite))){ i =>
      intercept[IntervalException] {
        i()
      }
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

    forAll(
      Table(("n", "v"),
            (0,   true),
            (8,   false),
            (10,  false))) { (n, v) =>
      _mito8.contains(n) should equal (v)
    }

    val _3topi = OpenInterval(3, pInfinite)

    forAll(
      Table(("n", "v"),
            (0,   false),
            (3,   false),
            (10,  true))) { (n, v) =>
      _3topi.contains(n) should equal (v)
    }
  }

  it should "support equals method." in {
    val _3to8 = OpenInterval(3, 8)

    forAll(
      Table(
        ("i",                  "v"),
        (OpenInterval(3, 8),   true),
        (OpenInterval(1, 6),   false),
        (ClosedInterval(3, 8), false),
        (ClosedInterval(1, 6), false))) { (i, v) =>
      _3to8.equals(i) should equal (v)
    }
  }

  it should "support isConnectedTo method." in {
    val _3topi = OpenInterval(3, pInfinite)

    forAll(
      Table(("i",                  "v"),
            (OpenInterval(1, 6),   true),
            (OpenInterval(1, 9),   true),
            (OpenInterval(6, 9),   true),
            (OpenInterval(1, 3),   false),
            (OpenInterval(8, 14),  true))) { (i, v) =>
      _3topi.isConnectedTo(i) should equal (v)
    }

    val _mito8 = OpenInterval(mInfinite, 8)

    forAll(
      Table(("i",                  "v"),
            (OpenInterval(1, 8),   true),
            (OpenInterval(8, 15),  false),
            (OpenInterval(9, 15),  false))) { (i, v) =>
      _mito8.isConnectedTo(i) should equal (v)
    }
  }

  it should "support containsAll method." in {
    val _3topi = OpenInterval(3, pInfinite)

    forAll(
      Table(("a",                  "v"),
            (Array(2, 3, 4, 7),    false),
            (Array(3, 4, 7),       false),
            (Array(4, 7),          true))) { (a, v) =>
      _3topi.containsAll(a) should equal (v)
    }
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

    forAll(
      Table(("s",         "v"),
            ("(-inf,8)",  "(-inf,8)"),
            ("(3,+inf)",  "(3,+inf)"))) { (s, v) =>
      OpenInterval.parse(s).toString should equal (v)
    }
  }
}
