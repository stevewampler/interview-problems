package com.sgw.utils

import java.text.{DateFormatSymbols, SimpleDateFormat}
import java.util.concurrent.TimeUnit._
import java.util.concurrent._
import java.util.{Date, Locale}

import scala.concurrent.duration.Duration
import scala.util.Try

trait TimeConversions extends Any {
  protected def timeIn(unit: TimeUnit): Time

  def nanoseconds  = timeIn(NANOSECONDS)
  def nanos        = nanoseconds
  def nanosecond   = nanoseconds
  def nano         = nanoseconds

  def microseconds = timeIn(MICROSECONDS)
  def micros       = microseconds
  def microsecond  = microseconds
  def micro        = microseconds

  def milliseconds = timeIn(MILLISECONDS)
  def millis       = milliseconds
  def millisecond  = milliseconds
  def milli        = milliseconds

  def seconds      = timeIn(SECONDS)
  def second       = seconds

  def minutes      = timeIn(MINUTES)
  def minute       = minutes

  def hours        = timeIn(HOURS)
  def hour         = hours

  def days         = timeIn(DAYS)
  def day          = days
}

/**
 * A factory for Time objects.
 */
object Time {
  /**
   * An implicit used to convert an integer to a time as in "1 second" or "5 minutes".
   *
   * @param n the value to convert
   */
  implicit final class TimeInt(val n: Int) extends AnyVal with TimeConversions {
    protected def timeIn(unit: TimeUnit): Time = Time(n, unit)
  }

  /**
   * An implicit used to convert a long to a time as in "1 second" or "5 minutes".
   *
   * @param n the value to convert
   */
  implicit final class TimeLong(val n: Long) extends AnyVal with TimeConversions {
    protected def timeIn(unit: TimeUnit): Time = Time(n, unit)
  }

  private lazy val YearMonthDayDateFormat = new SimpleDateFormat("YYYY-MM-dd", new DateFormatSymbols(Locale.US))
  private lazy val ISO8601DateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

  private lazy val SchedulerPoolSize = 10

  // This pool contains daemon threads only, so the app can shutdown (without having to remember to call
  // "shutdown" on the thread pool) even if the pool contains threads, scheduled or not.
  private lazy val Scheduler = Executors.newScheduledThreadPool(SchedulerPoolSize, new ThreadFactory() {
    @Override
    def newThread(runnable: Runnable): Thread = {
      val thread = Executors.defaultThreadFactory().newThread(runnable)
      thread.setDaemon(true) // make'm daemons
      thread
    }
  })

  lazy val ZERO     = nanos(0L)
  lazy val INFINITE = nanos(Long.MaxValue)
  lazy val INVALID  = INFINITE

//  val MILLIS_PER_SECOND = 1000
//  val MILLIS_PER_MINUTE = MILLIS_PER_SECOND * 60
//  val MILLIS_PER_HOUR   = MILLIS_PER_MINUTE * 60
//  val MILLIS_PER_DAY    = MILLIS_PER_HOUR * 24

  val NANOS_PER_MILLI  = 1e6
  val NANOS_PER_SECOND = NANOS_PER_MILLI * 1000
  val NANOS_PER_MINUTE = NANOS_PER_SECOND * 60
  val NANOS_PER_HOUR   = NANOS_PER_MINUTE * 60
  val NANOS_PER_DAY    = NANOS_PER_HOUR * 24

  def formatAsDate(date: Date): String = YearMonthDayDateFormat.synchronized {
    YearMonthDayDateFormat.format(date)
  }

  def formatAsDate(time: Time): String = formatAsDate(time.toDate)

  def parseAsYearMonthDay(date: String): Time = YearMonthDayDateFormat.synchronized {
    Time(YearMonthDayDateFormat.parse(date))
  }

  def formatAsISO8601(date: Date): String = ISO8601DateFormat.synchronized {
    ISO8601DateFormat.format(date)
  }

  def formatAsISO8601(time: Time): String = formatAsISO8601(time.toDate)

  def parseAsISO8601(date: String): Time = ISO8601DateFormat.synchronized {
    Time(ISO8601DateFormat.parse(date))
  }

  /**
   * Functions used to create Time object with various TimeUnits.
   * @param value the tiem value
   * @return a Time object
   */
  def nanos(value: Long)   = Time(value, NANOSECONDS)
  def micros(value: Long)  = Time(value, MICROSECONDS)
  def millis(value: Long)  = Time(value, MILLISECONDS)
  def seconds(value: Long) = Time(value, SECONDS)
  def minutes(value: Long) = Time(value, MINUTES)
  def hours(value: Long)   = Time(value, HOURS)
  def days(value: Long)    = Time(value, DAYS)
  def weeks(value: Long)   = days(value * 7)

  /**
   * Returns the current time as a Time object.
   *
   * @return the current time as a Time object.
   */
  def now: Time = nanos(System.nanoTime())

  /**
   * Returns a random time object between the specified fromTime (inclusive) and toTime (exclusive).
   *
   * @param fromTime the minimum time of the random time (inclusive)
   * @param toTime the maximum time of the random time (exclusive)
   *
   * @return a random time object between the specified fromTime (inclusive) and toTime (exclusive).
   */
  def random(fromTime: Time, toTime: Time): Time = fromTime + (toTime - fromTime).random

  def wrapInRunnable(f: () => Unit) = new Runnable { def run(): Unit = f() }

  /**
   * Creates and executes a one-shot action that becomes enabled
   * after the given delay.
   *
   * @param f the function to execute
   * @param delay the time from now to delay execution

   * @return a ScheduledFuture representing pending completion of
   *         the task and whose <tt>get()</tt> method will return
   *         <tt>null</tt> upon completion
   * @throws RejectedExecutionException if the task cannot be
   *                                    scheduled for execution
   * @throws NullPointerException if command is null
   */
  def schedule(f: () => Unit, delay: Time): ScheduledFuture[_] =
    Scheduler.schedule(wrapInRunnable(f), delay.time, delay.units)

  /**
   * Creates and executes a periodic action that becomes enabled first
   * after the given initial delay, and subsequently with the given
   * period; that is executions will commence after
   * <tt>initialDelay</tt> then <tt>initialDelay+period</tt>, then
   * <tt>initialDelay + 2 * period</tt>, and so on.
   * If any execution of the task
   * encounters an exception, subsequent executions are suppressed.
   * Otherwise, the task will only terminate via cancellation or
   * termination of the executor.  If any execution of this task
   * takes longer than its period, then subsequent executions
   * may start late, but will not concurrently execute.
   *
   * @param f the function to execute
   * @param initialDelay the time to delay first execution
   * @param period the period between successive executions
   * @return a ScheduledFuture representing pending completion of
   *         the task, and whose <tt>get()</tt> method will throw an
   *         exception upon cancellation
   * @throws RejectedExecutionException if the task cannot be
   *                                    scheduled for execution
   * @throws NullPointerException if command is null
   * @throws IllegalArgumentException if period less than or equal to zero
   */
  def scheduleAtFixedRate(f: () => Unit, initialDelay: Time, period: Time): ScheduledFuture[_] =
    Scheduler.scheduleAtFixedRate(wrapInRunnable(f), initialDelay.millis, period.millis, TimeUnit.MILLISECONDS)

  /**
   * Creates and executes a periodic action that becomes enabled first
   * after the given initial delay, and subsequently with the
   * given delay between the termination of one execution and the
   * commencement of the next.  If any execution of the task
   * encounters an exception, subsequent executions are suppressed.
   * Otherwise, the task will only terminate via cancellation or
   * termination of the executor.
   *
   * @param f the function to execute
   * @param initialDelay the time to delay first execution
   * @param delay the delay between the termination of one
   *              execution and the commencement of the next
   * @return a ScheduledFuture representing pending completion of
   *         the task, and whose <tt>get()</tt> method will throw an
   *         exception upon cancellation
   * @throws RejectedExecutionException if the task cannot be
   *                                    scheduled for execution
   * @throws NullPointerException if command is null
   * @throws IllegalArgumentException if delay less than or equal to zero
   */
  def scheduleWithFixedDelay(f: () => Unit, initialDelay: Time, delay: Time): ScheduledFuture[_] =
    Scheduler.scheduleWithFixedDelay(wrapInRunnable(f), initialDelay.millis, delay.millis, TimeUnit.MILLISECONDS)

  /**
   * Converts a java Date to a Time.
   * @param date the date
   * @return the Time represented by the millis of the specified date
   */
  def apply(date: Date): Time = millis(date.getTime)
}

/**
 * An object representing a point in time.
 *
 * @param time the time value
 * @param units the time units
 */
case class Time(time: Long, units: TimeUnit = NANOSECONDS) extends Comparable[Time] {
  def isNanos   = units == NANOSECONDS
  def isMicros  = units == MICROSECONDS
  def isMillis  = units == MILLISECONDS
  def isSeconds = units == SECONDS
  def isMinutes = units == MINUTES
  def isHours   = units == HOURS
  def isDays    = units == DAYS

  def toNanos   = convertTo(NANOSECONDS)
  def toMicros  = convertTo(MICROSECONDS)
  def toMillis  = convertTo(MILLISECONDS)
  def toSeconds = convertTo(SECONDS)
  def toMinutes = convertTo(MINUTES)
  def toHours   = convertTo(HOURS)
  def toDays    = convertTo(DAYS)
  def toDate    = new Date(millis)

  def nanos   = toNanos.time
  def micros  = toMicros.time
  def millis  = toMillis.time
  def seconds = toSeconds.time
  def minutes = toMinutes.time
  def hours   = toHours.time
  def days    = toDays.time

  def random: Time = this * scala.math.random

  def +(rhs: Time): Time = Time.nanos(nanos + rhs.nanos)
  def -(rhs: Time): Time = Time.nanos(nanos - rhs.nanos)
  def /(rhs: Time): Time = Time.nanos(nanos / rhs.nanos)
  def *(rhs: Time): Time = Time.nanos(nanos * rhs.nanos)

  def /(timeNanos: Double): Time = Time.nanos((nanos / timeNanos).toLong)
  def *(timeNanos: Double): Time = Time.nanos((nanos * timeNanos).toLong)

  def +(timeNanos: Long): Time = Time.nanos(nanos + timeNanos)
  def -(timeNanos: Long): Time = Time.nanos(nanos - timeNanos)
  def /(timeNanos: Long): Time = Time.nanos(nanos / timeNanos)
  def *(timeNanos: Long): Time = Time.nanos(nanos * timeNanos)

  def isZero: Boolean = time == 0
  def isNotZero: Boolean = !isZero
  def isPositive: Boolean = time > 0
  def isPositiveOrZero: Boolean = time >= 0
  def isNegative: Boolean = time < 0
  def isNegativeOrZero: Boolean = time <= 0
  def isInfinite: Boolean = time == Long.MaxValue
  def isNotInfinite: Boolean = !isInfinite
  def isInvalid: Boolean = this == Time.INVALID

  def ==(timeNanos: Long): Boolean = nanos == timeNanos
  def < (timeNanos: Long): Boolean = nanos <  timeNanos
  def > (timeNanos: Long): Boolean = nanos >  timeNanos
  def <=(timeNanos: Long): Boolean = nanos <= timeNanos
  def >=(timeNanos: Long): Boolean = nanos >= timeNanos

  def ==(rhs: Time): Boolean = nanos == rhs.nanos
  def < (rhs: Time): Boolean = nanos <  rhs.nanos
  def > (rhs: Time): Boolean = nanos >  rhs.nanos
  def <=(rhs: Time): Boolean = nanos <= rhs.nanos
  def >=(rhs: Time): Boolean = nanos >= rhs.nanos

  def compareTo(rhs: Time): Int = nanos.compareTo(rhs.nanos)

  def min(rhs: Time): Time = Time(nanos.min(rhs.nanos), NANOSECONDS)

  def max(rhs: Time): Time = Time(nanos.max(rhs.nanos), NANOSECONDS)

  def toSimplifiedUnits: Time = {
    val n = nanos

    if (n == 0) toSeconds
    else if (n % Time.NANOS_PER_DAY    == 0) toDays
    else if (n % Time.NANOS_PER_HOUR   == 0) toHours
    else if (n % Time.NANOS_PER_MINUTE == 0) toMinutes
    else if (n % Time.NANOS_PER_SECOND == 0) toSeconds
    else if (n % Time.NANOS_PER_MILLI  == 0) toMillis
    else toNanos
  }

  def sleep(): Try[Unit] = Try { if (isPositive) Thread.sleep(millis) }

  def convertTo(toUnit: TimeUnit): Time = if (units == toUnit) this else Time(toUnit.convert(time, units), toUnit)

  def toDouble(toUnit: TimeUnit): Double = toUnit match {
    case TimeUnit.NANOSECONDS => nanos.toDouble
    case TimeUnit.MICROSECONDS => micros.toDouble
    case TimeUnit.MILLISECONDS => millis.toDouble
    case TimeUnit.SECONDS => seconds.toDouble
    case TimeUnit.MINUTES => minutes.toDouble
    case TimeUnit.HOURS => hours.toDouble
    case TimeUnit.DAYS => days.toDouble
  }

  def toDuration = Duration(time, units)

  def roundUp: Time = {
    val n = nanos

    if (n == 0) toNanos
    else if (n < Time.NANOS_PER_MILLI)  toMillis
    else if (n < Time.NANOS_PER_SECOND) toSeconds
    else if (n < Time.NANOS_PER_MINUTE) toMinutes
    else if (n < Time.NANOS_PER_HOUR)   toHours
    else if (n < Time.NANOS_PER_DAY)    toDays
    else toDays
  }

  def toMap: Map[String, Any] = Map("time" -> time, "units" -> units)

  def delta(to: Time): Time = if (this.isInvalid || to.isInvalid) Time.INVALID else { to - this }

  def age: Time = delta(Time.now)

  lazy val toDateString: String = Time.formatAsDate(this)

  lazy val toISO8601DateString: String = Time.formatAsISO8601(this)

  def singularUnitsString: String = pluralUnitsString.substring(0, pluralUnitsString.length - 1)

  def pluralUnitsString: String = units.toString.toLowerCase

  def unitsString: String = if (time == 1L) singularUnitsString else pluralUnitsString

  def toStringImpl: String = time.toString + " " + unitsString

  override def toString: String = toSimplifiedUnits.toStringImpl

  def toString(units: TimeUnit): String = convertTo(units).toString
}


