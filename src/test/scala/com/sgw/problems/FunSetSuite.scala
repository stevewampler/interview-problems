package com.sgw.problems

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    *  - test
    *  - ignore
    *  - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }

  test("contains is implemented") {
    assert(FunSet(x => true).contains(1), 100)
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    *   val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1 = FunSet(1)
    val s2 = FunSet(2)
    val s3 = FunSet(3)
    val s4 = FunSet(4)
    val s123 = s1.union(s2.union(s3))
  }

//  new TestSets {
//    FunSet.printSet(s1)
//    FunSet.printSet(s2)
//    FunSet.printSet(s3)
//    FunSet.printSet(s4)
//    FunSet.printSet(s123)
//  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(s1.contains(1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      assert(s123.contains(1), "Union 1")
      assert(s123.contains(2), "Union 2")
      assert(s123.contains(3), "Union 3")
      assert(!s123.contains(4), "Union 4")
    }
  }

  test("intersection contains all elements common to both sets") {
    new TestSets {
      val s = s123.intersect(s1)
      assert(s.contains(1), "Intersect 1")
      assert(!s.contains(2), "Intersect 2")
      assert(!s.contains(3), "Intersect 3")
      assert(!s.contains(4), "Intersect 4")
    }
  }

  test("diff contains elements in one set minus those in another") {
    new TestSets {
      val s = s123.diff(s2)
      assert(s.contains(1), "Diff 1")
      assert(!s.contains(2), "Diff 2")
      assert(s.contains(3), "Diff 3")
      assert(!s.contains(4), "Diff 4")
    }
  }

  test("filter contains all elements common to the set and the filter") {
    new TestSets {
      val s = s123.filter(_ == 1)
      assert(s.contains(1), "Filter 1")
      assert(!s.contains(2), "Filter 2")
      assert(!s.contains(3), "Filter 3")
      assert(!s.contains(4), "Filter 4")
    }
  }

  test("forall should return true if all of the items in the set pass the predicate") {
    new TestSets {
      assert(s123.forall(_ => true))
    }
  }

  test("forall should return false if at least of the items in the set fails the predicate") {
    new TestSets {
      assert(!s123.forall(_ != 2))
    }
  }

  test("exists should return true if at least one of the items in the set passes the predicate") {
    new TestSets {
      assert(s123.exists(_ == 2))
    }
  }

  test("exists should return false if none of the items in the set passes the predicate") {
    new TestSets {
      assert(!s123.exists(_ => false))
    }
  }

  test("map should return a set that transforms the s123 set into a s567 set") {
    new TestSets {
      val s567 = s123.map(_ + 4)
      assert(!s567.contains(1), "Contains 1")
      assert(!s567.contains(2), "Contains 2")
      assert(!s567.contains(3), "Contains 3")
      assert(!s567.contains(4), "Contains 4")
      assert(s567.contains(5), "Contains 5")
      assert(s567.contains(6), "Contains 6")
      assert(s567.contains(7), "Contains 7")
      assert(!s567.contains(8), "Contains 8")
    }
  }
}

