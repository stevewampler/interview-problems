package com.sgw.futures

import org.specs2.execute.Result
import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Random, Failure, Success}

class FuturesSpec extends Specification with ThrownExpectations {
  "A successful Scala Future" should {
    "run on its own thread" in Result.unit {
      val mainThreadName = Thread.currentThread().getName

      val aFuture = future {
        Thread.currentThread().getName
      }

      Thread.sleep(500) // to give the future a chance to run

      aFuture.onComplete {
        case Success(futureThreadName) => {
          val currentThreadName = Thread.currentThread.getName // may or may not be the same thread on which the future ran
          println(s"mainThreadName=$mainThreadName, futureThreadName=$futureThreadName, currentThreadName=$currentThreadName")
          futureThreadName must_!= mainThreadName
          currentThreadName must_!= mainThreadName
        }
        case Failure(ex) => failure(ex.getMessage)
      }
    }
  }

  "A failed Scala Future" should {
    "fail" in Result.unit {
      val aFuture = future {
        throw new RuntimeException("Failed!")
      }

      aFuture.onComplete {
        case Success(futureThreadName) => failure("Should have failed!")
        case Failure(ex) => ex.getMessage must be_==("Failed!")
      }
    }
  }

  "A Scala Future" should {
    "not be referentially transparent" in Result.unit {
      val aFuture = future {
        Random.nextLong()
      }

      aFuture.onComplete {
        case Success(num1) => aFuture.onComplete {
          case Success(num2) => num1 must be_==(num2)
          case Failure(ex) => failure(ex.getMessage)
        }
        case Failure(ex) => failure(ex.getMessage)
      }

      // aFuture and what aFuture was set equal to don't produce the same result
      aFuture.onComplete {
        case Success(num1) => future {
          Random.nextLong()
        }.onComplete {
          case Success(num2) => num1 must be_!==(num2)
          case Failure(ex) => failure(ex.getMessage)
        }
        case Failure(ex) => failure(ex.getMessage)
      }
    }
  }
}
