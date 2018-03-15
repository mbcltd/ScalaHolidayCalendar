package org.jfin

import java.time.{LocalDate, Month}

object UnitedKingdom {
  val settlement = HolidayCalendar(
    List(
      NewYearsDay.followingWeekend(),
      GoodFriday,
      EasterMonday,
      FirstMondayOfMay,
      LastMondayOfMay.exceptInYears(List(2002,2012)),
      LastMondayOfAugust,
      ChristmasDay,
      BoxingDay,
      SpecificDay(LocalDate.of(2002,6,3),"Golden Jubilee Bank Holiday"),
      SpecificDay(LocalDate.of(2002,6,4),"Special Spring Bank Holiday"),
      SpecificDay(LocalDate.of(2011,4,29),"Royal Wedding Bank Holiday"),
      SpecificDay(LocalDate.of(2012,6,4),"Diamond Jubilee Bank Holiday"),
      SpecificDay(LocalDate.of(2012,6,5),"Special Spring Bank Holiday"),
      SpecificDay(LocalDate.of(1999,12,31),"Millennium New Years Eve")
    ),"gb"
  )
}
