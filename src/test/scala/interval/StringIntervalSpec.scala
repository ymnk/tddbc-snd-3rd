package interval

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks._

import Point.pointType._

class StringIntervalSpec extends FlatSpec with BeforeAndAfter with ShouldMatchers {

  before {
  }

  after {
  }

  behavior of "String Interval"
  it should "be instantiated by lower and upper points" in {
    OpenInterval("B", "D") 
  }

  it should "impelement the pretty printer." in {
    OpenInterval("B", "D").toString should equal ("(B,D)")
  }


  it should "throw an exception if its arguments are wrong." in {
    intercept[IntervalException] {
      OpenInterval("D", "B") 
    }
  }


  it should "return its lower point." in {
    OpenInterval("B", "D").lowerPoint should equal ("B": Point)
  }

  it should "return its upper point." in {
    OpenInterval("B", "D").upperPoint should equal ("D": Point)
  }


  it should "contains method." in {
    val interval = OpenInterval("B", "D")

    forAll(
      Table(
        ("n", "v"),
        ("C",   true),
        ("B",   false),
        ("A",   false))){ (n, v) =>
      interval.contains(n) should equal (v)
    }
  }

  it should "support equals method." in {
    val _btod = OpenInterval("B", "D")

    forAll(
      Table(
        ("i",                      "v"),
        (OpenInterval("B", "D"),   true),
        (OpenInterval("A", "C"),   false),
        (ClosedInterval("B", "D"), false),
        (ClosedInterval("A", "C"), false))){ (i, v) =>
      _btod.equals(i) should equal (v)
    }
  }

  it should "support isConnectedTo method." in {
    val _btod = OpenInterval("B", "D")

    forAll(
      Table(
        ("i",                      "v"),
        (OpenInterval("A", "C"),   true),
        (OpenInterval("A", "E"),   true),
        (OpenInterval("C", "E"),   true),
        (OpenInterval("A", "B"),   false),
        (OpenInterval("D", "F"),   false),
        (OpenInterval("E", "F"),   false))){ (i, v) =>
      _btod.isConnectedTo(i) should equal (v)
    }
  }

  it should "support isConnectedTo method for other intervals." in {
    val _btod = OpenInterval("B", "D")

    forAll(
      Table(
        ("i",                          "v"),
        (ClosedInterval("A", "B"),     false),
        (ClosedInterval("D", "F"),     false),
        (OpenInterval("A", "B"),       false),
        (OpenInterval("D", "F"),       false),
        (ClosedOpenInterval("A", "B"), false),
        (ClosedOpenInterval("D", "F"), false),
        (OpenClosedInterval("A", "B"), false),
        (OpenClosedInterval("D", "F"), false))){ (i, v) =>
      _btod.isConnectedTo(i) should equal (v)
    }
  }

  it should "support containsAll method." in {
    val _btod = OpenInterval("B", "D")

    forAll(
      Table(
        ("a",                  "v"),
        (Array("C", "E", "B"), false),
        (Array("C", "A"),      false),
        (Array("C"),           true))){ (a, v) =>
      _btod.containsAll(a) should equal (v)
    }
  }

  it should "support getIntersection method for other intervals." in {
    val _btoe = OpenInterval("B", "E")

    forAll(
      Table("i",
            OpenInterval("A", "B"),
            ClosedInterval("A", "B"),
            OpenClosedInterval("A", "B"),
            ClosedOpenInterval("A", "B"))) { i =>
      intercept[IntervalException] {
        _btoe.getIntersection(i) //  "(B,B)"
      }
    }

    forAll(
      Table(
        ("i",                            "v"),
        (OpenInterval("B", "C"),         "(B,C)"),
        (ClosedInterval("B", "C"),       "(B,C]"),
        (OpenClosedInterval("B", "C"),   "(B,C]"),
        (ClosedOpenInterval("B", "C"),   "(B,C)"),

        (OpenInterval("B", "E"),         "(B,E)"),
        (ClosedInterval("B", "E"),       "(B,E)"),
        (OpenClosedInterval("B", "E"),   "(B,E)"),
        (ClosedOpenInterval("B", "E"),   "(B,E)"),

        (OpenInterval("B", "F"),         "(B,E)"),
        (ClosedInterval("B", "F"),       "(B,E)"),
        (OpenClosedInterval("B", "F"),   "(B,E)"),
        (ClosedOpenInterval("B", "F"),   "(B,E)"),

        (OpenInterval("C", "D"),         "(C,D)"),
        (ClosedInterval("C", "D"),       "[C,D]"),
        (OpenClosedInterval("C", "D"),   "(C,D]"),
        (ClosedOpenInterval("C", "D"),   "[C,D)"),

        (OpenInterval("C", "E"),         "(C,E)"),
        (ClosedInterval("C", "E"),       "[C,E)"),
        (OpenClosedInterval("C", "E"),   "(C,E)"),
        (ClosedOpenInterval("C", "E"),   "[C,E)"),

        (OpenInterval("C", "F"),         "(C,E)"),
        (ClosedInterval("C", "F"),       "[C,E)"),
        (OpenClosedInterval("C", "F"),   "(C,E)"),
        (ClosedOpenInterval("C", "F"),   "[C,E)"))){ (i, v) =>
      _btoe.getIntersection(i).toString should equal (v)
    }

    val intervals =
      OpenClosedInterval(mInfinite, "E") ::
      ClosedOpenInterval("B", pInfinite) ::
      OpenInterval("B", pInfinite) ::
      OpenInterval(mInfinite, "E") ::
      List(OpenInterval.apply _,
           ClosedInterval.apply _,
           OpenClosedInterval.apply _,
           ClosedOpenInterval.apply _).flatMap { f =>
        List(("A","B"), 
             ("B","D"), ("B","E"), ("B","F"),
             ("C","D"), ("C","E"), ("C","F"),
             ("E","F")).map { p =>
        f(p._1, p._2)
      }
    }

    for(i <- Array("A","B","C","D","E","F","G")) {
      for(ival1 <- intervals) {
        for(ival2 <- intervals) {
          if(!ival2.isConnectedTo(ival1)){
            intercept[IntervalException] {
              ival2.getIntersection(ival1)
            }
          }
          else if(ival2.contains(i) && ival1.contains(i)){
            ival2.getIntersection(ival1).contains(i) should equal (true)
          }
          else {
            ival2.getIntersection(ival1).contains(i) should equal (false)
          }
        } 
      }
    }
  }

  it should "support parse method." in {
    OpenInterval.parse("(B,D)").toString should equal ("(B,D)")
  }
}
