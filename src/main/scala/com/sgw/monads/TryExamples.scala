package com.sgw.monads

import scala.util.{Failure, Success, Try}

object TryExamples extends App {
  // The Try monad is a function alternative to the "try" statement

  {
    // Try, like Option, has two possible values: Success and Failure
    val successfulTry: Try[String] = Success("Hello, World!")
    val failedTry: Try[String] = Failure(new RuntimeException("Failed!"))

    println(successfulTry)
    println(failedTry)
  }

  {
    // instead of ...
    try {
      ??? // throws NotImplementedError
    } catch {
      case ex: Throwable => println(ex)
    } finally {
      println("finally!")
    }

    // you can use a Try ...
    val aFailedTry: Try[String] = Try {
      ??? // throws NotImplementedError
    }

    println(aFailedTry.isSuccess)
    println(aFailedTry.isFailure)

    // if the Try is successful, you can do stuff with it, like calling foreach to get the Try's value ...
    aFailedTry.map { str =>
      s"Hello, $str"
    }.foreach { str =>
      println(str)
    }

    // you can also recover from a failure (which is equiv. to "map" for failures) ...
    aFailedTry.recover {
      case _: NotImplementedError => "Steve"
      case ex => println(ex)
    }.map { str =>
      s"Hello, $str"
    }.foreach { str =>
      println(str)
    }

    // recoverWith is like flatMap for failures

    // here's a successful try ...
    val aSuccessfulTry: Try[String] = Try {
      "Steve"
    }

    // that you can do stuff with
    aSuccessfulTry.recover {
      case _: NotImplementedError => "recover will not be called on a successful try"
      case ex => println(ex)
    }.map { str =>
      s"Hello, $str"
    }.foreach { str =>
      println(str)
    }

    // lots of other container like things you can do on Try objects as well, like
    // * filtering
    // * converting to an Option
    // * flatMap'ing
    // * folding
    // * ...
  }

  {
    println("--------------")

    // we can implement our own Retry { ... } that's similar to Try { ... } ...
    object Retry {
      // side note 1: an "apply" method can be called with or without the word "apply"
      //   i.e. Retry.apply(...) is the same as Retry(...) { ... }
      // side note 2: you call this function like this: Retry(...) { ... } not Retry(...)(...) because the 2nd parameter list is a function.
      // side note 3: f: => T is a "by-name parameter" that will not be called/evaluated until the function f is actually called in the code below
      def apply[T](numTries: Int, delayMillis: Long)(f: => T): Try[T] = Try[T] {
        println(s"Trying with $numTries tries remaining ...")
        f // call the function
      }.recoverWith[T] { case ex => // recover == map, recoverWith -> flatMap
        println(s"Try failed. Number of tries remaining = ${numTries - 1}")
        if (numTries <= 1) {
          // we've tried our best, give up
          Failure(ex)
        } else {
          // we still have some tries left, so try again ...
          println(s"Sleeping for $delayMillis milliseconds ...")
          Thread.sleep(delayMillis)

          apply[T](numTries - 1, delayMillis)(f)
        }
      }
    }

    val result: Try[String] = Retry(numTries = 3, delayMillis = 500) { // this function is the argument to the 2nd parameter list even though it's using {} instead of ()
      println("Doing something that might fail ...")
      ??? // throws NotImplementedError
    }

    println(result)
  }
}
