package org.jfin

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.scalatest.{FreeSpec, MustMatchers}

class UnitedKingdomTest extends FreeSpec with MustMatchers {


  "United Kingdom" - {
    "Should have the correct holidays for 2019" in {
      val bankHolidays = List(
        ("2019-01-01","New Years Day"),
        ("2019-04-19","Good Friday"),
        ("2019-04-22","Easter Monday"),
        ("2019-05-06","First Monday of May"),
        ("2019-05-27","Last Monday of May"),
        ("2019-08-26","Last Monday of August"),
        ("2019-12-25","Christmas Day"),
        ("2019-12-26","Boxing Day")
      ).map(  f => (LocalDate.parse(f._1),f._2) )

      val cal = UnitedKingdom.settlement

      for( d <- bankHolidays) {
        cal.holiday( d._1 ).map( _.name ) mustBe Some(d._2)
      }

    }

    "Should have the correct holidays for 2016" in {
      val bankHolidays = List(
        ("2016-01-01","New Years Day"),
        ("2016-03-25","Good Friday"),
        ("2016-03-28","Easter Monday"),
        ("2016-05-02","First Monday of May"),
        ("2016-05-30","Last Monday of May"),
        ("2016-08-29","Last Monday of August"),
        ("2016-12-26","Boxing Day"),
        ("2016-12-27","Christmas Day")
      ).map(  f => (LocalDate.parse(f._1),f._2) )

      val cal = UnitedKingdom.settlement

      for( d <- bankHolidays) {
        cal.holiday( d._1 ).map( _.name ) mustBe Some(d._2)
      }

    }
  }
}
