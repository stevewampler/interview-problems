package com.sgw.parser

import com.sgw.parser.ComplexParser.WorkflowParser.WorkflowAST

import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

/**
  * An example of a more complex scala-parser-combinator.
  *
  * Can parse the following DSL:
  *
  * <block> ::= (<statement>)+
  *  <statement> ::= "exit"
  *            | "read input" (<identifier> ",")* <identifier>
  *            | "call service" <stringLiteral>
  *            | "switch" ":" INDENT (<ifThen>)+ [otherwiseThen] DEDENT
  *
  * <ifThen> ::= <condition> "->" INDENT <block> DEDENT
  *
  * <otherwiseThen> ::= "otherwise" "->" INDENT <block> DEDENT
  *
  * <condition> ::= <identifier> "==" <stringLiteral>
  *
  * E.g. from:
  *
  * read input name, country
  * switch:
  *   country == "PT" ->
  *     call service "A"
  *     exit
  *   otherwise ->
  *     call service "B"
  *     switch:
  *       name == "unknown" ->
  *         exit
  *       otherwise ->
  *         call service "C"
  *         exit
  *
  * to this:
  *
  * AndThen(
  *   ReadInput(List(name, country)),
  *   Choice(List(
  *     IfThen(
  *       Equals(country, PT),
  *       AndThen(CallService(A), Exit)
  *     ),
  *     OtherwiseThen(
  *       AndThen(
  *         CallService(B),
  *         Choice(List(
  *           IfThen(Equals(name, unknown), Exit),
  *           OtherwiseThen(AndThen(CallService(C), Exit))
  *         ))
  *       )
  *     )
  *   ))
  * )
  *
  * See: https://enear.github.io/2016/03/31/parser-combinators/
  */
object ComplexParser {
  sealed trait WorkflowToken extends Positional

  case class IDENTIFIER(str: String) extends WorkflowToken
  case class LITERAL(str: String) extends WorkflowToken
  case class INDENTATION(spaces: Int) extends WorkflowToken
  case class EXIT() extends WorkflowToken
  case class READINPUT() extends WorkflowToken
  case class CALLSERVICE() extends WorkflowToken
  case class SWITCH() extends WorkflowToken
  case class OTHERWISE() extends WorkflowToken
  case class COLON() extends WorkflowToken
  case class ARROW() extends WorkflowToken
  case class EQUALS() extends WorkflowToken
  case class COMMA() extends WorkflowToken
  case class INDENT() extends WorkflowToken
  case class DEDENT() extends WorkflowToken

  case class Location(line: Int, column: Int) {
    override def toString = s"$line:$column"
  }

  trait WorkflowCompilationError
  case class WorkflowLexerError(location: Location, msg: String) extends WorkflowCompilationError
  case class WorkflowParserError(location: Location, msg: String) extends WorkflowCompilationError

  object WorkflowLexer extends RegexParsers {
    override def skipWhitespace = true
    override val whiteSpace = "[ \t\r\f]+".r

    // parser for identifiers
    def identifier: Parser[IDENTIFIER] = positioned {
      "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
    }

    // parsers for string literals and indentations
    def literal: Parser[LITERAL] = positioned {
      """"[^"]*"""".r ^^ { str =>
        val content = str.substring(1, str.length - 1)
        LITERAL(content)
      }
    }

    def indentation: Parser[INDENTATION] = positioned {
      "\n[ ]*".r ^^ { whitespace =>
        val nSpaces = whitespace.length - 1
        INDENTATION(nSpaces)
      }
    }

    // keyword parsers
    def exit          = positioned { "exit"          ^^ (_ => EXIT()) }
    def readInput     = positioned { "read input"    ^^ (_ => READINPUT()) }
    def callService   = positioned { "call service"  ^^ (_ => CALLSERVICE()) }
    def switch        = positioned { "switch"        ^^ (_ => SWITCH()) }
    def otherwise     = positioned { "otherwise"     ^^ (_ => OTHERWISE()) }
    def colon         = positioned { ":"             ^^ (_ => COLON()) }
    def arrow         = positioned { "->"            ^^ (_ => ARROW()) }
    def equals        = positioned { "=="            ^^ (_ => EQUALS()) }
    def comma         = positioned { ","             ^^ (_ => COMMA()) }

    def tokens: Parser[List[WorkflowToken]] = {
      phrase(rep1(exit | readInput | callService | switch | otherwise | colon | arrow
        | equals | comma | literal | identifier | indentation)) ^^ { rawTokens =>
        processIndentations(rawTokens)
      }
    }

    private def processIndentations(
      tokens: List[WorkflowToken],
      indents: List[Int] = List(0)
    ): List[WorkflowToken] = {
      tokens.headOption match {

        // if there is an increase in indentation level, we push this new level into the stack
        // and produce an INDENT
        case Some(INDENTATION(spaces)) if spaces > indents.head =>
          INDENT() :: processIndentations(tokens.tail, spaces :: indents)

        // if there is a decrease, we pop from the stack until we have matched the new level,
        // producing a DEDENT for each pop
        case Some(INDENTATION(spaces)) if spaces < indents.head =>
          val (dropped, kept) = indents.partition(_ > spaces)
          (dropped map (_ => DEDENT())) ::: processIndentations(tokens.tail, kept)

        // if the indentation level stays unchanged, no tokens are produced
        case Some(INDENTATION(spaces)) if spaces == indents.head =>
          processIndentations(tokens.tail, indents)

        // other tokens are ignored
        case Some(token) =>
          token :: processIndentations(tokens.tail, indents)

        // the final step is to produce a DEDENT for each indentation level still remaining, thus
        // "closing" the remaining open INDENTS
        case None =>
          indents.filter(_ > 0).map(_ => DEDENT())

      }
    }

    def apply(code: String): Either[WorkflowLexerError, List[WorkflowToken]] = {
      parse(tokens, code) match {
        case NoSuccess(msg, next) =>
          Left(WorkflowLexerError(Location(next.pos.line, next.pos.column), msg))
        case Success(result, next) =>
          Right(result)
      }
    }
  }

  object WorkflowParser extends Parsers {
    override type Elem = WorkflowToken

    class WorkflowTokenReader(tokens: Seq[WorkflowToken]) extends Reader[WorkflowToken] {
      override def first: WorkflowToken = tokens.head
      override def atEnd: Boolean = tokens.isEmpty
      override def pos: Position = NoPosition
      override def rest: Reader[WorkflowToken] = new WorkflowTokenReader(tokens.tail)
    }

    sealed trait WorkflowAST extends Positional
    case class AndThen(step1: WorkflowAST, step2: WorkflowAST) extends WorkflowAST
    case class ReadInput(inputs: Seq[String]) extends WorkflowAST
    case class CallService(serviceName: String) extends WorkflowAST
    case class Choice(alternatives: Seq[ConditionThen]) extends WorkflowAST
    case object Exit extends WorkflowAST

    sealed trait ConditionThen extends Positional { def thenBlock: WorkflowAST }
    case class IfThen(predicate: Condition, thenBlock: WorkflowAST) extends ConditionThen
    case class OtherwiseThen(thenBlock: WorkflowAST) extends ConditionThen

    sealed trait Condition extends Positional
    case class Equals(factName: String, factValue: String) extends Condition

    private def identifier: Parser[IDENTIFIER] = positioned {
      accept("identifier", { case id @ IDENTIFIER(name) => id })
    }

    private def literal: Parser[LITERAL] = positioned {
      accept("string literal", { case lit @ LITERAL(name) => lit })
    }

    def condition: Parser[Equals] = positioned {
      (identifier ~ EQUALS() ~ literal) ^^ { case id ~ eq ~ lit => Equals(id.str, lit.str) }
    }

    def program: Parser[WorkflowAST] = positioned {
      phrase(block)
    }

    def block: Parser[WorkflowAST] = positioned {
      rep1(statement) ^^ { case stmtList => stmtList reduceRight AndThen }
    }

    def statement: Parser[WorkflowAST] = positioned {
      val exit = EXIT() ^^ (_ => Exit)
      val readInput = READINPUT() ~ rep(identifier ~ COMMA()) ~ identifier ^^ {
        case read ~ inputs ~ IDENTIFIER(lastInput) => ReadInput(inputs.map(_._1.str) ++ List(lastInput))
      }
      val callService = CALLSERVICE() ~ literal ^^ {
        case call ~ LITERAL(serviceName) => CallService(serviceName)
      }
      val switch = SWITCH() ~ COLON() ~ INDENT() ~ rep1(ifThen) ~ opt(otherwiseThen) ~ DEDENT() ^^ {
        case _ ~ _ ~ _ ~ ifs ~ otherwise ~ _ => Choice(ifs ++ otherwise)
      }
      exit | readInput | callService | switch
    }

    def ifThen: Parser[IfThen] = {
      (condition ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
        case cond ~ _ ~ _ ~ block ~ _ => IfThen(cond, block)
      }
    }

    def otherwiseThen: Parser[OtherwiseThen] = {
      (OTHERWISE() ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
        case _ ~ _ ~ _ ~ block ~ _ => OtherwiseThen(block)
      }
    }

    def apply(tokens: Seq[WorkflowToken]): Either[WorkflowParserError, WorkflowAST] = {
      val reader = new WorkflowTokenReader(tokens)
      program(reader) match {
        case NoSuccess(msg, next) =>
          Left(WorkflowParserError(Location(next.first.pos.line, next.first.pos.column), msg))
        case Success(result, next) =>
          Right(result)
      }
    }
  }

  object WorkflowCompiler {
    def apply(code: String): Either[WorkflowCompilationError, WorkflowAST] = {
      for {
        tokens <- WorkflowLexer(code).right
        ast <- WorkflowParser(tokens).right
      } yield ast
    }
  }

  def main(args: Array[String]) {
    val goodCode =
      """
        |read input name, country
        |switch:
        |  country == "PT" ->
        |    call service "A"
        |    exit
        |  otherwise ->
        |    call service "B"
        |    switch:
        |      name == "unknown" ->
        |        exit
        |      otherwise ->
        |        call service "C"
        |        exit
      """.stripMargin.trim

    val badCode =
      """
        |read input name, country
        |switch:
        |  country == PT ->
        |    call service "A"
        |    exit
        |  otherwise ->
        |    call service "B"
        |    switch:
        |      name == "unknown" ->
        |        exit
        |      otherwise ->
        |        call service "C"
        |        exit
      """.stripMargin.trim

    //    val lexerResult = WorkflowLexer(goodCode)
    //
    //    println(lexerResult)

    val goodCompilerResult = WorkflowCompiler(goodCode)

    println(goodCompilerResult)

    val badCompilerResult = WorkflowCompiler(badCode)

    println(badCompilerResult)
  }
}
