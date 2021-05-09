package Assignment1.Problem1

import java.time.{DayOfWeek, LocalDate}

object NumberOfDays extends App {

  /** Returns the number of days of type 'dayOfWeek' occuring on 'day' of each month
   * between 'fromDate' and 'toDate'
   *
   * e.g. to get Sundays that fell on the first of the month during the twentieth century
   * (1 Jan 1901 to 31 Dec 2000), the parameters would be
   * fromDate - 1 Jan 1901
   * toDate - 31 Dec 2000
   * dayOfWeek - Sunday
   * day - 1
   */
  def getNumberOfDays(fromDate: LocalDate, toDate: LocalDate, dayOfWeek: DayOfWeek, day:Int): Int = {
    if (fromDate == null || toDate == null || fromDate.compareTo(toDate) > 0 || dayOfWeek == null || day > 31 || day < 1 )
      throw new IllegalArgumentException
    val occurencesOfDay =
    for (year <- fromDate.getYear to toDate.getYear; month <- 1 to 12;
         date =  LocalDate.of(year,month,day)
         if (date.getDayOfWeek == dayOfWeek && date.compareTo(fromDate) >=0 && date.compareTo(toDate) <= 0 )
    ) yield 1
    occurencesOfDay.size
  }

  val fromDate = LocalDate.of(1901,1,1)
  val toDate = LocalDate.of(2000,12,31)
  println(getNumberOfDays(fromDate,toDate,DayOfWeek.SUNDAY,1))
}