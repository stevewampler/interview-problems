package com.sgw.scalaz

import org.specs2.mutable.Specification
import org.specs2.execute.Result

import java.util.concurrent.ScheduledThreadPoolExecutor

import scala.util.Random
import scalaz.{-\/, \/-}
import scalaz.concurrent.Task

/**
 * Learning me some Scalaz: The Task Monad.
 */
class TaskSpec extends Specification {
  implicit val stpe = new ScheduledThreadPoolExecutor(8)

  "A successful Task" should {
    "run without an error on a separate thread" in Result.unit {
      val task = Task { Thread.currentThread() }

      val result = task.unsafePerformSyncAttempt

      result.isRight must beTrue

      result.foreach(thread => thread must_!= Thread.currentThread())
    }
  }

  "A failed Task" should {
    "fail" in Result.unit {
      val task = Task { throw new RuntimeException("Fail!") }

      task.unsafePerformSyncAttempt.isLeft must beTrue
    }
  }

  "A flat-mapped set of Tasks" should {
    "succeed" in Result.unit {
      val chainOfTasks = for {
        r1 <- Task("A")
        r2 <- Task(r1 + "B")
        r3 <- Task(r2 + "C")
        r4 <- Task(r3 + "D")
      } yield r4

      val result = chainOfTasks.unsafePerformSyncAttempt

      result.isRight must beTrue

      result.foreach(_ must be_==("ABCD"))
    }
  }

  "A flat-mapped set of Tasks" should {
    "run on the same thread" in Result.unit {
      val chain = for {
        t1 <- Task(Thread.currentThread())
        t2 <- {
          t1 must_== Thread.currentThread()
          Task(Thread.currentThread())
        }
        t3 <- {
          t2 must_== Thread.currentThread()
          Task(Thread.currentThread())
        }
        t4 <- {
          t3 must_== Thread.currentThread()
          Task(Thread.currentThread())
        }
      } yield t4

      val result = chain.unsafePerformSyncAttempt

      result.isRight must beTrue

      result.foreach(thread => thread must_!= Thread.currentThread())
    }
  }

  "A folded set of flatMapped Tasks" should {
    "not run on the main thread" in Result.unit {
      val numTasks = 10

      val chain = (0 until numTasks).foldLeft(Task(List[String](Thread.currentThread.getName))) {
        (task, _) => task.flatMap(list => Task(Thread.currentThread.getName :: list))
      }

      val result = chain.unsafePerformSyncAttempt

      result.isRight must beTrue

      result.foreach(threadNames => {
        threadNames.size must_== numTasks + 1
        // threadNames.foreach(println)
        threadNames.contains(Thread.currentThread.getName) must beFalse
      })
    }
  }

  "A flat-mapped set of Tasks where one in the middle fails" should {
    "fail" in Result.unit {
      val chainOfTasks = for {
        r1 <- Task("A")
        r2 <- Task(r1 + "B")
        r3 <- Task[String](throw new RuntimeException("Fail!"))
        r4 <- Task(r3 + "D")
      } yield r4

      val result = chainOfTasks.unsafePerformSyncAttempt

      result.isRight must beFalse
    }
  }

  "A flat-mapped set of Tasks" should {
    "run sequentially" in Result.unit {
      def addTime(times: List[Long]): Task[List[Long]] = Task {
        Thread.sleep(100)
        System.currentTimeMillis() :: times
      }

      val startTime = System.currentTimeMillis()

      // val chainOfTasks = addTime(List(startTime)).flatMap(times => addTime(times)).flatMap(times => addTime(times))

      val chainOfTasks = for {
        times1 <- addTime(List(startTime))
        times2 <- addTime(times1)
        times3 <- addTime(times2)
      } yield times3

      chainOfTasks.unsafePerformSyncAttempt.map(times => times.foldLeft((true, Long.MaxValue)) {
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

      task.unsafePerformSyncAttempt.map(_ must be_==(1)).isRight must beTrue
    }
  }

  "Task.fail" should {
    "lift a throwable into a Task" in {
      Task.fail(new RuntimeException("Fail!")).unsafePerformSyncAttempt.isLeft must beTrue
    }
  }

  "Task.gatherUnordered" should {
    "run multiple tasks at roughly the same time" in Result.unit {
      val tasks = (0 to 5).map(i => Task {
        val startTime = System.currentTimeMillis()
        Thread.sleep(100)
        startTime
      })
      Task.gatherUnordered(tasks).unsafePerformSyncAttempt.map(list => list.exists(item => list.head - item > 50) must beFalse).isRight must beTrue
    }

    "run multiple tasks on different threads" in Result.unit {
      val tasks = (0 until 5).map(i => Task {
        Thread.currentThread()
      })
      Task.gatherUnordered(tasks).unsafePerformSyncAttempt.map(list => {
        val numUniqueThreads = list.toSet.size
        numUniqueThreads must be_>(1) // we should have used at least two threads
      }).isRight must beTrue
    }
  }

  "A timed Task" should {
    "error out if the task takes too long" in {
      Task { Thread.sleep(300) }.timed(100).unsafePerformSyncAttempt.isLeft must beTrue
    }
  }
}
