package com.sgw.scalaz

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

import scalaz.State

/**
 * Learning me some Scalaz
 */
class StateSpec extends FlatSpec with Matchers with BeforeAndAfter {
  // from http://eed3si9n.com/learning-scalaz/State.html
  "The scalaz State" should "be able to be used to implement a stack" in {
    type Stack = List[Int]

    // defining pop and push state functions

    val pop = State[Stack, Int] { case x :: xs => (xs, x) }

    def push(x: Int) = State[Stack, Unit] { case xs => (x :: xs, ()) }

    def stackManip: State[Stack, Int] = for {
      _ <- push(3)
      a <- pop
      b <- pop
    } yield b

    val initialState = List(5, 8, 2, 1)

    stackManip.run(initialState) should be((List(8, 2, 1),5))

    // defining pop and push using get and put

    def init[S]: State[S, S] = State(s => (s, s))
    def get[S]: State[S, S] = init
    def put[S](s: S): State[S, Unit] = State(_ => (s, ()))

    val pop2: State[Stack, Int] = for {
      s <- get[Stack]
      (x :: xs) = s
      _ <- put(xs)
    } yield x

    def push2(x: Int): State[Stack, Unit] = for {
      xs <- get[Stack]
      r <- put(x :: xs)
    } yield r

    def stackManip2: State[Stack, Int] = for {
      _ <- push2(3)
      a <- pop2
      b <- pop2
    } yield b

    stackManip2.run(List(5, 8, 2, 1)) should be((List(8, 2, 1),5))

    // using flatMaps instead of a for comp.

    def stackManip3: State[Stack, Int] = push(3).flatMap(_ => pop).flatMap(_ => pop)

    stackManip3.run(List(5, 8, 2, 1)) should be((List(8, 2, 1),5))
  }
}
