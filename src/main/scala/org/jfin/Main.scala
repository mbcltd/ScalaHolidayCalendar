package org.jfin

import java.time.format.DateTimeFormatter

object Main extends App {
  val calendar = SettlementCalendars.gb

  for (r <- calendar.rules) {
    println(s"${r.name}: ${r.describe}")
  }

  println()

  for (h <- calendar.allHolidaysForYear(2019)) {
    println(s"${h.date.format(DateTimeFormatter.ISO_LOCAL_DATE)}: ${h.rule.name}")
  }


}
