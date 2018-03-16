package org.jfin

import java.time.format.{DateTimeFormatter, TextStyle}
import java.time.{DayOfWeek, LocalDate, Month}
import java.util.Locale

trait HolidayRule {
  def isHoliday(d: LocalDate): Option[Holiday]

  def name: String

  def describe: String

  def following(following: HolidayRule): HolidayRule = Following(this, following)

  def followingWeekend(): HolidayRule = Following(this, Weekend)

  def exceptInYear(year: Int): HolidayRule = exceptInYears(List(year))

  def exceptInYears(years: List[Int]): HolidayRule = ExceptInYears(this, years)

  def onlyInYear(year: Int): HolidayRule = onlyInYears(List(year))

  def onlyInYears(years: List[Int]): HolidayRule = OnlyInYears(this, years)

  def ++(b: HolidayRule): HolidayRule = CompoundHolidayRule(this, b)

  def describeNumber(n: Int): String = s"$n${numberSuffix(n)}"

  def describeMonth(m: Month): String = m.getDisplayName(TextStyle.FULL, Locale.UK)

  def numberSuffix(n: Int): String = if (n > 3 && n < 21) "th" else n % 10 match {
    case 1 => "st"
    case 2 => "nd"
    case 3 => "rd"
    case _ => "th"
  }
}

trait SimpleHolidayRule extends HolidayRule {
  def condition(d: LocalDate): Boolean

  override val name: String

  override def isHoliday(d: LocalDate): Option[Holiday] = if (condition(d)) Some(Holiday(d, this)) else None
}

case class SpecificDay(date: LocalDate, name: String) extends SimpleHolidayRule {
  override def condition(d: LocalDate): Boolean = d.isEqual(date)

  override def describe: String = date.format(DateTimeFormatter.ISO_LOCAL_DATE)
}

case class SpecificDayOfYear(dayOfMonth: Int, month: Month, name: String) extends SimpleHolidayRule {
  override def condition(d: LocalDate): Boolean = d.getDayOfMonth == dayOfMonth && d.getMonth == month

  override def describe: String = s"${describeNumber(dayOfMonth)} of ${describeMonth(month)}"
}

case class Following(rule: HolidayRule, following: HolidayRule) extends HolidayRule {

  def isHoliday(d: LocalDate): Option[Holiday] =
    if (following.isHoliday(d).isDefined)
      isHoliday(d.plusDays(1))
    else
      rule.isHoliday(d)

  override val name: String = rule.name

  override def describe: String = s"${rule.describe} or first day following ${following.describe}"
}

case class CompoundHolidayRule(a: HolidayRule, b: HolidayRule) extends HolidayRule {
  override def isHoliday(d: LocalDate): Option[Holiday] = a.isHoliday(d).orElse(b.isHoliday(d))

  override def describe: String = s"${a.describe} and ${b.describe}"

  override val name: String = s"${a.name} and ${b.name}"
}

case class ExceptInYears(rule: HolidayRule, excludeYears: List[Int]) extends HolidayRule {
  override def isHoliday(d: LocalDate): Option[Holiday] = if (excludeYears.contains(d.getYear)) None else rule.isHoliday(d)

  override def describe: String = s"${rule.describe} except in ${excludeYears.mkString(", ")}"

  override val name: String = rule.name
}

case class OnlyInYears(rule: HolidayRule, includeYears: List[Int]) extends HolidayRule {
  override def isHoliday(d: LocalDate): Option[Holiday] = if (includeYears.contains(d.getYear)) rule.isHoliday(d) else None

  override def describe: String = s"${rule.describe} in ${includeYears.mkString(", ")}"

  override val name: String = rule.name
}

object Weekend extends SimpleHolidayRule {
  override def condition(d: LocalDate): Boolean = d.getDayOfWeek == DayOfWeek.SATURDAY || d.getDayOfWeek == DayOfWeek.SUNDAY

  override val name = "Weekend"

  override def describe: String = name
}

case class SpecificDate(date: LocalDate, name: String) extends SimpleHolidayRule {
  def condition(d: LocalDate): Boolean = d.isEqual(date)

  override def describe: String = date.format(DateTimeFormatter.ISO_LOCAL_DATE)
}

object GoodFriday extends SimpleHolidayRule {
  override def condition(d: LocalDate): Boolean = Easter.goodFridays.get(d.getYear).contains(d.getDayOfYear)

  override val name: String = "Good Friday"

  override def describe: String = "Friday before Easter Day"
}

object EasterMonday extends SimpleHolidayRule {
  override def condition(d: LocalDate): Boolean = Easter.easterMondays.get(d.getYear).contains(d.getDayOfYear)

  override val name: String = "Easter Monday"

  override def describe: String = "Monday after Easter Day"
}

case class FirstMondayOfMonth(month: Month, name: String) extends SimpleHolidayRule {
  override def condition(d: LocalDate): Boolean =
    d.getMonth == month && d.getDayOfMonth <= 7 && d.getDayOfWeek == DayOfWeek.MONDAY

  override def describe: String = s"First Monday of ${describeMonth(month)}"
}

case class LastMondayOfMonth(month: Month, name: String) extends SimpleHolidayRule {
  override def condition(d: LocalDate): Boolean =
    d.getMonth == month && d.getDayOfMonth >= d.lengthOfMonth() - 6 && d.getDayOfWeek == DayOfWeek.MONDAY

  override def describe: String = s"Last Monday of ${describeMonth(month)}"
}

object ChristmasDayFollowingWeekend extends SimpleHolidayRule {


  override def condition(d: LocalDate): Boolean = d.isEqual(christmasDayForYear(d.getYear))

  def christmasDayForYear(y: Int): LocalDate = LocalDate.of(y, 12, 25).getDayOfWeek match {
    case DayOfWeek.SATURDAY => LocalDate.of(y, 12, 27)
    case DayOfWeek.SUNDAY => LocalDate.of(y, 12, 27)
    case _ => LocalDate.of(y, 12, 25)
  }

  override val name: String = "Christmas Day"

  override def describe: String = s"25th of December or first day following weekend"
}

object BoxingDayFollowingWeekend extends SimpleHolidayRule {
  override def condition(d: LocalDate): Boolean = d.isEqual(boxingDayForYear(d.getYear))

  def boxingDayForYear(y: Int): LocalDate = LocalDate.of(y, 12, 26).getDayOfWeek match {
    case DayOfWeek.SATURDAY => LocalDate.of(y, 12, 28)
    case DayOfWeek.SUNDAY => LocalDate.of(y, 12, 28)
    case _ => LocalDate.of(y, 12, 26)
  }

  override val name: String = "Boxing Day"

  override def describe: String = s"26th of December or first day following weekend"
}


object Easter {
  val easterMondayDaysOfYears: List[Int] = List(
    98, 90, 103, 95, 114, 106, 91, 111, 102, // 1901-1909
    87, 107, 99, 83, 103, 95, 115, 99, 91, 111, // 1910-1919
    96, 87, 107, 92, 112, 103, 95, 108, 100, 91, // 1920-1929
    111, 96, 88, 107, 92, 112, 104, 88, 108, 100, // 1930-1939
    85, 104, 96, 116, 101, 92, 112, 97, 89, 108, // 1940-1949
    100, 85, 105, 96, 109, 101, 93, 112, 97, 89, // 1950-1959
    109, 93, 113, 105, 90, 109, 101, 86, 106, 97, // 1960-1969
    89, 102, 94, 113, 105, 90, 110, 101, 86, 106, // 1970-1979
    98, 110, 102, 94, 114, 98, 90, 110, 95, 86, // 1980-1989
    106, 91, 111, 102, 94, 107, 99, 90, 103, 95, // 1990-1999
    115, 106, 91, 111, 103, 87, 107, 99, 84, 103, // 2000-2009
    95, 115, 100, 91, 111, 96, 88, 107, 92, 112, // 2010-2019
    104, 95, 108, 100, 92, 111, 96, 88, 108, 92, // 2020-2029
    112, 104, 89, 108, 100, 85, 105, 96, 116, 101, // 2030-2039
    93, 112, 97, 89, 109, 100, 85, 105, 97, 109, // 2040-2049
    101, 93, 113, 97, 89, 109, 94, 113, 105, 90, // 2050-2059
    110, 101, 86, 106, 98, 89, 102, 94, 114, 105, // 2060-2069
    90, 110, 102, 86, 106, 98, 111, 102, 94, 114, // 2070-2079
    99, 90, 110, 95, 87, 106, 91, 111, 103, 94, // 2080-2089
    107, 99, 91, 103, 95, 115, 107, 91, 111, 103, // 2090-2099
    88, 108, 100, 85, 105, 96, 109, 101, 93, 112, // 2100-2109
    97, 89, 109, 93, 113, 105, 90, 109, 101, 86, // 2110-2119
    106, 97, 89, 102, 94, 113, 105, 90, 110, 101, // 2120-2129
    86, 106, 98, 110, 102, 94, 114, 98, 90, 110, // 2130-2139
    95, 86, 106, 91, 111, 102, 94, 107, 99, 90, // 2140-2149
    103, 95, 115, 106, 91, 111, 103, 87, 107, 99, // 2150-2159
    84, 103, 95, 115, 100, 91, 111, 96, 88, 107, // 2160-2169
    92, 112, 104, 95, 108, 100, 92, 111, 96, 88, // 2170-2179
    108, 92, 112, 104, 89, 108, 100, 85, 105, 96, // 2180-2189
    116, 101, 93, 112, 97, 89, 109, 100, 85, 105 // 2190-2199
  )

  val easterMondays: Map[Int, Int] = (1901 to 2199).zip(easterMondayDaysOfYears).toMap
  val goodFridays: Map[Int, Int] = easterMondays.mapValues(_ - 3)

}