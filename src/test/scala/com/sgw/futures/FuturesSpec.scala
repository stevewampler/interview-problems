package com.sgw.futures

import org.specs2.execute.Result
import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Random, Success}

class FuturesSpec extends Specification with ThrownExpectations {
  // Scala futures require an ExecutionContext in scope.
  // The default "global" execution context limits the number of concurrent futures to the number of CPUs, which is
  // good for worker threads doing computations, but not so good for I/O bound threads.
  import ExecutionContext.Implicits.global

  sequential // execute test sequentially

  "The Future companion object" should {
    "enable you to declare a successful future" in Result.unit {
      val future: Future[String] = Future.successful("Hello, World")

      future.isCompleted shouldEqual true

      future.onComplete {
        case Success(value) => println(value)
        case Failure(ex) => failure(s"Should have succeeded! ex = $ex")
      }

      // ignore this for now ...
      Await.result(future, Duration.Inf)
    }

    "enable you to declare a failed future" in Result.unit {
      val future: Future[Nothing] = Future.failed(new RuntimeException("Failed!"))

      future.isCompleted shouldEqual true

      future.onComplete {
        case Success(_) => failure("Should have failed!")
        case Failure(ex) => println(ex.getMessage)
      }

      // ignore this for now ...
      Await.result(future, Duration.Inf)
    }
  }

  "A successful Scala Future" should {
    "run on its own thread" in Result.unit {
      val mainThreadName = Thread.currentThread().getName

      // like "Try" you can create a new "Future" like so ...
      val aFuture: Future[String] = Future {
        val futureThreadName = Thread.currentThread().getName
        Thread.sleep(500) // if you wait too long, you're app could end before your future completes!
        futureThreadName
      }

      // you can setup a callback on your Future like so ...
      // this does not cause a wait!
      aFuture.onComplete {
        case Success(futureThreadName) => {
          val currentThreadName = Thread.currentThread.getName // may or may not be the same thread on which the future ran
          println(s"mainThreadName=$mainThreadName, futureThreadName=$futureThreadName, currentThreadName=$currentThreadName")
          futureThreadName must_!= mainThreadName
          currentThreadName must_!= mainThreadName
        }
        case Failure(ex) => failure(ex.getMessage)
      }

      println("before 2nd onComplete")

      Thread.sleep(1000)

      aFuture.onComplete {
        case Success(_) =>
          println("2nd time")
        case Failure(exception) => exception.printStackTrace()
      }

      println("Waiting for future to hopefully complete ...")

      // to give the future a chance to complete
      Thread.sleep(1000)

      println("Should be done by now.")
    }

    "be awaitable" in Result.unit {
      val aFuture: Future[String] = Future {
        val futureThreadName = Thread.currentThread().getName
        Thread.sleep(500) // if you wait too long, you're app could end before your future completes!
        futureThreadName
      }

      println("Waiting on future ...")

      val result: String = Await.result(aFuture, Duration.Inf)

      println(s"result = $result")
    }

    "be mappable and flatMap-able" in Result.unit {
      val aFuture: Future[String] = Future {
        s"before map, thread=${Thread.currentThread().getName}"
      }.map { str: String =>
        str + s", after map, thread=${Thread.currentThread().getName}"
      }.flatMap { str =>
        Future.successful(s"$str, flatMap thread=${Thread.currentThread().getName}")
      }

      val result: String = Await.result(aFuture, Duration.Inf)

      println(result)
    }
  }

  "A failed Scala Future" should {
    "fail" in Result.unit {
      val aFuture = Future {
        throw new RuntimeException("Failed!")
      }

      aFuture.onComplete {
        case Success(_) => failure("Should have failed!")
        case Failure(ex) => ex.getMessage must be_==("Failed!")
      }
    }
  }
}
