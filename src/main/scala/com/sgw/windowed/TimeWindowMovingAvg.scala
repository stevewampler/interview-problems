package com.sgw.windowed

import java.time.{Clock, Duration, Instant}

import com.sgw.utils.{Logging, Timer}

object TimeWindowMovingAvg extends App with Logging {

  // (time, value, expected average)
  List[(Long, Double, Double)](
    (1L, 1.0, 1.0/1000),
    (2L, 1.0, 2.0/1000),
    (3L, 4.0, 6.0/1000),
    (3L, 4.0, 10.0/1000),
    (1001L, 0.0, 9.0/1000),
    (1002L, 4.0, 12.0/1000),
    (1003L, 0.0, 4.0/1000)
  ).foldLeft(
    TimeWindowMovingAvg[Double](
      windowSize = Duration.ofMillis(1000),
      total = 0.0
    )
  ) { case (movingAvg, (time, value, expectedAvg)) =>
    Timer {
      movingAvg.add(now = Instant.ofEpochMilli(time), value)
    }.map { case (timerTime, newMovingAvg) =>

      info(newMovingAvg)

      info(s"time = $timerTime")
      info(s"${newMovingAvg.perMilli} == $expectedAvg ?")

      newMovingAvg
    }.recover { case ex =>
      error(s"Failed!", ex)
      movingAvg
    }.get
  }

  List[(Long, Int, Double)](
    (1L, 1, 1.0/1000),
    (2L, 1, 2.0/1000),
    (3L, 4, 6.0/1000),
    (3L, 4, 10.0/1000),
    (1001L, 0, 9.0/1000),
    (1002L, 4, 12.0/1000),
    (1003L, 0, 4.0/1000)
  ).foldLeft(
    TimeWindowMovingAvg[Int](
      windowSize = Duration.ofMillis(1000),
      total = 0
    )
  ) { case (movingAvg, (time, value, expectedAvg)) =>
    Timer {
      movingAvg.add(now = Instant.ofEpochMilli(time), value)
    }.map { case (timerTime, newMovingAvg) =>

      info(newMovingAvg)

      info(s"time = $timerTime")
      info(s"${newMovingAvg.perMilli} == $expectedAvg ?")

      newMovingAvg
    }.recover { case ex =>
      error(s"Failed!", ex)
      movingAvg
    }.get
  }
}

case class TimeWindowMovingAvg[T](
  windowSize: Duration,
  queue: Queue[(Instant, T)] = Queue.empty[(Instant, T)],
  total: T
)(implicit num: Numeric[T]) {

  // make the implicit Numeric conversions available
  import num._

  require(!windowSize.isZero)
  require(!windowSize.isNegative)

  def add(now: Instant, value: T): TimeWindowMovingAvg[T] = {

    val minInstant = now.minus(windowSize)

    val (oldValues, newQueue1) = queue.dequeueWhile { case (instant, _) =>
      instant == minInstant || instant.isBefore(minInstant)
    }

    val oldTotal: T = oldValues.map { case (_, oldValue) =>
      oldValue
    }.sum

    val newQueue2 = newQueue1.enqueue((now, value))

    val newTotal = total - oldTotal + value

    copy(queue = newQueue2, total = newTotal)
  }

  def add(value: T)(implicit clock: Clock): TimeWindowMovingAvg[T] = add(clock.instant(), value)

  def perNano: Double = total.toDouble() / windowSize.toNanos
  def perMilli: Double = total.toDouble() / windowSize.toMillis
  def perSecond: Double = perMilli / 1000.0
  def perMinute: Double = total.toDouble() / windowSize.toMinutes
  def perHour: Double = total.toDouble() / windowSize.toHours
  def perDay: Double = total.toDouble() / windowSize.toDays
}
