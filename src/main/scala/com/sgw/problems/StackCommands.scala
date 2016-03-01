package com.sgw.problems

/**
 * Given an array of stack command strings, where the first item in the array is the number of subsequent commands,
 * parse the commands, apply them to a stack, and print out the top of the stack. If the stack is empty, print "EMPTY".
 */
object StackCommands extends App {
  val commands = Array(
    "6",
    "push 5",
    "pop",
    "push 10",
    "push 11",
    "pop",
    "push 20"
  )

  val n = commands.headOption.map(_.toInt).getOrElse(0) // get the number of subsequent commands

  val (builder, _) =
    commands.
      drop(1). // drop the command count
      take(n). // take the n commands
      map(_.split(" ").toList). // parse the commands, and fold over them to build up the result string and maintain the stack
      foldLeft((new StringBuilder, List[String]())) {
        case ((builder, stack), List("push", v)) => (builder.append(v).append('\n'), v :: stack)
        case ((builder, stack), List("pop")) if stack.nonEmpty => {
          val newStack = stack.tail
          (builder.append(newStack.headOption.getOrElse("EMPTY")).append('\n'), newStack)
        }
        case ((builder, stack), List("pop")) => (builder.append(stack.headOption.getOrElse("EMPTY")).append('\n'), stack)
        case ((_, _), List(cmd, _)) => throw new RuntimeException(s"unknown command: $cmd")
        case ((_, _), List(cmd)) => throw new RuntimeException(s"unknown command: $cmd")
      }

  println(builder.toString())
}
