package com.sgw.scalaz

import org.specs2.mutable.Specification
import org.specs2.execute.Result

import java.util.concurrent.ScheduledThreadPoolExecutor

import scalaz.concurrent.Task

/**
 * Learning me some Scalaz: The Task Monad.
 */
class TaskSpec extends Specification {
  implicit val stpe = new ScheduledThreadPoolExecutor(8)

  "A successful Task" should {
    "run without an error" in Result.unit {
      val testThreadName = Thread.currentThread().getName

      val task = Task { s"Hello World from thread ${Thread.currentThread().getName}, run from thread $testThreadName." }

      val attemptedRun = task.attemptRun

      attemptedRun.isRight must beTrue

      attemptedRun.foreach(println)
    }
  }

  "A failed Task" should {
    "fail" in Result.unit {
      val task = Task { throw new RuntimeException("Fail!") }

      val attemptedRun = task.attemptRun

      attemptedRun.isLeft must beTrue
    }
  }

  "A flat-mapped set of Tasks" should {
    "run sequentially" in Result.unit {
      def addTime(times: List[Long]): Task[List[Long]] = Task {
                                                                Thread.sleep(100)
                                                                System.currentTimeMillis() :: times
                                                              }

      val startTime = System.currentTimeMillis()

      val chain = addTime(List(startTime)).flatMap(times => addTime(times)).flatMap(times => addTime(times))

      chain.attemptRun.map(times => times.foldLeft((true, Long.MaxValue)) {
        case ((isOrdered, currentTime), time) => (isOrdered && currentTime > time, time)
      }).map {
        case (isOrdered, _) => isOrdered must beTrue
      }.isRight must beTrue

      val endTime = System.currentTimeMillis()

      (endTime - startTime) must be_>=(300L)
    }
  }

  "Task.now" should {
    "lift a value into a Task" in Result.unit {
      val task = Task.now(1)

      task.attemptRun.map(_ must be_==(1)).isRight must beTrue
    }
  }

  "Task.fail" should {
    "lift a throwable into a Task" in {
      Task.fail(new RuntimeException("Fail!")).attemptRun.isLeft must beTrue
    }
  }

  "Task.gatherUnordered" should {
    "run multiple tasks at roughly the same time" in Result.unit {
      val tasks = (0 to 5).map(i => Task {
        val startTime = System.currentTimeMillis()
        Thread.sleep(100)
        startTime
      })
      Task.gatherUnordered(tasks).attemptRun.map(list => list.exists(item => list.head - item > 50) must beFalse).isRight must beTrue
    }
  }

  "A timed Task" should {
    "error out if the task takes too long" in {
      Task { Thread.sleep(300) }.timed(100).attemptRun.isLeft must beTrue
    }
  }
}
