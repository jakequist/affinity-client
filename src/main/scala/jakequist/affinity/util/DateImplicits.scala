package jakequist.affinity.util

import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.time.Instant
import java.util.concurrent.TimeUnit

import org.joda.time
import org.joda.time._
import org.joda.time.format.{PeriodFormat, PeriodFormatterBuilder}

import scala.concurrent.duration.Duration.Infinite
import scala.concurrent.duration.{Duration, FiniteDuration}


trait DateImplicits {


  implicit def instantToTimestamp(base:Instant) : Timestamp = Timestamp.from(base)

  implicit def sqlDateToJoda(base: java.sql.Date): DateTime = new DateTime(base.getTime)

  implicit def jodaToSqlDate(base: DateTime): java.sql.Date = base.toSql

  implicit def jodaToTimestamp(base: DateTime): java.sql.Timestamp = new Timestamp(base.toEpoch)

  implicit def jodaToJavaDate(base: DateTime): java.util.Date = base.toJava

  implicit def jodaToEpochDate(base: DateTime): Long = base.toJava.getTime

  implicit def optionTimestampToDate(base: Option[Timestamp]): Option[java.sql.Date] = base.map(_.toSql)

  implicit def dateUtilOrdering: Ordering[java.util.Date] = Ordering.by(_.getTime)

  implicit def dateSqlOrdering: Ordering[java.sql.Date] = Ordering.by(_.getTime)


  implicit class TimestampImp(base: java.sql.Timestamp) {
    def toJoda: DateTime = new DateTime(base.getTime)
  }


  implicit class InstantImp(base: Instant) {

    def -(that: Instant): FiniteDuration = {
      Duration.apply(base.toEpochMilli - that.toEpochMilli, TimeUnit.MILLISECONDS)
    }

    def elapsed: FiniteDuration = {
      Instant.now() - base
    }


  }


  implicit class UtilDateImp(base: java.util.Date) {


    def toJoda: DateTime = new DateTime(base)

    def toSql: java.sql.Date = {
      base match {
        case v: java.sql.Date => v
        case _ => new java.sql.Date(base.getTime)
      }
    }

    def toTimestamp: java.sql.Timestamp = {
      new Timestamp(base.getTime)
    }

    def formatFullWithTimeZone: String = DateImplicits.MM_DD_YYYY_HHMMSS_Z.format(base)

    def formatRFC3339: String = DateImplicits.RFC3339.format(base)

    def format__YYYYMMDDHHMMSS: String = DateImplicits.YYYYMMDDHHMMSS.format(base)

    def format__YYYYMMDD_dot_HHMM: String = DateImplicits.YYYYMMDD_dot_HHMM.format(base)

    def format__YYYYMMDD_HHMM: String = DateImplicits.YYYYMMDD_HHMM.format(base)

    def format__YYYYMMDD: String = DateImplicits.YYYYMMDD.format(base)

    def format__DD_MM_YYYY: String = DateImplicits.MM_DD_YYYY.format(base)

    def format__YYYY_MM_DD_HH_MM: String = DateImplicits.YYYY_MM_DD_HH_MM.format(base)

    def format__YYYY_MM_DD: String = DateImplicits.YYYY_MM_DD.format(base)

    def from(that: java.util.Date): org.joda.time.Duration = {
      new org.joda.time.Duration(toJoda, new DateTime(that))
    }

    def fromNow: org.joda.time.Duration = {
      new org.joda.time.Duration(toJoda, null)
    }

    def fromNowAbs = {
      Duration.apply(
        Math.abs(fromNow.getMillis),
        TimeUnit.MILLISECONDS
      )
    }


    def ===(that: java.util.Date): Boolean = base.getTime == that.getTime

    def <(that: DateTime): Boolean = base.getTime < that.getMillis

    def <(that: java.util.Date): Boolean = base.getTime < that.getTime

    def <=(that: DateTime): Boolean = base.getTime <= that.getMillis

    def <=(that: java.util.Date): Boolean = base.getTime <= that.getTime

    def >(that: DateTime): Boolean = base.getTime > that.getMillis

    def >(that: java.util.Date): Boolean = base.getTime > that.getTime

    def >=(that: DateTime): Boolean = base.getTime >= that.getMillis

    def >=(that: java.util.Date): Boolean = base.getTime >= that.getTime

    def -(that: java.util.Date): Duration = {
      try {
        Duration.apply(base.getTime - that.getTime, TimeUnit.MILLISECONDS)
      } catch {
        case e: Throwable => throw new Exception(s"unable to get duration from ${that} ${base}")
      }
    }

    def +(that: Duration): java.sql.Date = {
      new java.sql.Date(base.getTime + that.toMillis)
    }


  }


  implicit class JodaDurationImp(base: org.joda.time.Duration) {


    def toScala: FiniteDuration = {
      Duration.apply(base.getMillis, TimeUnit.MILLISECONDS)
    }

//    def ago(implicit clock: Clock = Clock.default): DateTime = {
//      clock.now.minus(base)
//    }
//
//    def fromNow(implicit clock: Clock = Clock.default): DateTime = {
//      clock.now.plus(base)
//    }

    def formatFull: String = {
      PeriodFormat.getDefault().print(base.toPeriod())
    }

    def formatConcise: String = {
      DateImplicits.conciseFormatter.print(base.toPeriod)
    }
  }


  implicit class DurationImp(base: Duration) {


    def toJoda: org.joda.time.Duration = {
      org.joda.time.Duration.millis(base.toMillis)
    }

    def toYearsDouble: Double = base.toMillis.toDouble / (1000.0 * 60 * 60 * 24 * 365).toDouble

    def toDaysDouble: Double = base.toMillis.toDouble / (1000.0 * 60 * 60 * 24).toDouble

    def toHoursDouble: Double = base.toMillis.toDouble / (1000.0 * 60 * 60).toDouble

    def toMinutesDouble: Double = base.toMillis.toDouble / (1000.0 * 60).toDouble

    def toMillisInf: Long = {
      base match {
        case v: FiniteDuration => v.toMillis
        case v: Infinite => Long.MaxValue
      }
    }

//    def ago(implicit clock: Clock = Clock.default): DateTime = {
//      clock.now.minus(base)
//    }
//
//    def fromNow(implicit clock: Clock = Clock.default): DateTime = {
//      clock.now.plus(base)
//    }
//
//    def ahead(implicit clock: Clock = Clock.default): DateTime = {
//      clock.now.plus(base)
//    }
  }



  implicit class JodaDateTimeImp(base: DateTime) {

    def withFirstOfYear : DateTime = {
      base.withMonthOfYear(1).withDayOfMonth(1).withTimeAtStartOfDay()
    }

    def monthsFromNow = {
      monthsBetween(DateTime.now())
    }

    def daysFromNow = {
      daysBetween(DateTime.now())
    }

    def monthsBetween(that:DateTime) = {
      Math.abs( Months.monthsBetween(base.withTimeAtStartOfDay(), that.withTimeAtStartOfDay()).getMonths )
    }

    def daysBetween(that:DateTime) = {
      Math.abs( Days.daysBetween(base.withTimeAtStartOfDay(), that.withTimeAtStartOfDay()).getDays )
    }

    def hoursBetween(that:DateTime) = {
      Math.abs( Hours.hoursBetween(base.withTimeAtStartOfDay(), that.withTimeAtStartOfDay()).getHours )
    }

    def secondsBetween(that:DateTime) = {
      Math.abs( Seconds.secondsBetween(base.withTimeAtStartOfDay(), that.withTimeAtStartOfDay()).getSeconds )
    }

    def between(that:DateTime) = {
      if (base < that) {
        new time.Duration(base, that)
      } else {
        new time.Duration(that, base)
      }
    }

    def < (that:DateTime) : Boolean = {
      base.isBefore(that)
    }

    def <= (that:DateTime) : Boolean = {
      base.isEqual(that) || base.isBefore(that)
    }

    def > (that:DateTime) : Boolean = {
      base.isAfter(that)
    }

    def >= (that:DateTime) : Boolean = {
      base.isEqual(that) || base.isAfter(that)
    }


    def toSql: java.sql.Date = {
      new java.sql.Date(base.getMillis)
    }

    def toJava: java.util.Date = {
      new java.util.Date(base.getMillis)
    }

    def toTimestamp: java.sql.Timestamp = {
      new java.sql.Timestamp(base.getMillis)
    }

    def toEpoch: Long = {
      base.getMillis
    }

    def isWithin(duration: org.joda.time.Duration, now: DateTime): Boolean = {
      if (base.plus(duration).isBefore(now)) {
        true
      } else {
        false
      }
    }

//    def isWithin(duration: org.joda.time.Duration)(implicit clock: Clock): Boolean = {
//      isWithin(duration, clock.now)
//    }


  }



  implicit class JodaDuraitonImp(base: org.joda.time.Duration) {

    def toScala: FiniteDuration = {
      Duration.apply(base.getMillis, TimeUnit.MILLISECONDS)
    }

//    def ago(implicit clock: Clock = Clock.default): DateTime = {
//      clock.now.minus(base)
//    }
//
//    def fromNow(implicit clock: Clock = Clock.default): DateTime = {
//      clock.now.plus(base)
//    }
  }


  implicit def dcalaDurationToJodaDuration(duration: scala.concurrent.duration.Duration) : org.joda.time.Duration = {
    duration match {
      case v:FiniteDuration =>
        org.joda.time.Duration.millis(v.toMillis)
      case v:Infinite =>
        org.joda.time.Duration.millis(Long.MaxValue)
    }
  }




}

object DateImplicits extends DateImplicits {

  lazy val YYYYMMDD_HHMM = new SimpleDateFormat("yyyyMMdd_HHmm")
  lazy val YYYYMMDD_dot_HHMM = new SimpleDateFormat("yyyyMMdd.HHmm")
  lazy val YYYYMMDDHHMMSS = new SimpleDateFormat("yyyyMMddHHmmss")
  lazy val YYYYMMDD = new SimpleDateFormat("yyyyMMdd")
  lazy val MM_DD_YYYY = new SimpleDateFormat("MM-dd-yyyy")
  lazy val YYYY_MM_DD_HH_MM = new SimpleDateFormat("yyyy-MM-dd-HH-mm")
  lazy val YYYY_MM_DD = new SimpleDateFormat("yyyy-MM-dd")
  lazy val MM_DD_YYYY_HHMMSS_Z = new SimpleDateFormat("MM-dd-yyyy:HH:mm:ss Z")


  // 2011-06-03T10:00:00-07:00
  lazy val RFC3339 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX")


  lazy val conciseFormatter = {
    new PeriodFormatterBuilder()
      .appendDays()
      .appendSuffix("d")
      .appendHours()
      .appendSuffix("h")
      .appendMinutes()
      .appendSuffix("m")
      .appendSeconds()
      .appendSuffix("s")
      .toFormatter()
  }
}