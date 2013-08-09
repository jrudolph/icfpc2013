package net.virtualvoid.program

import org.parboiled.scala._
import org.parboiled.errors.ErrorUtils

object Parser extends org.parboiled.scala.Parser with BVParserRules {
  def apply(input: String): Program = {
    val result = ReportingParseRunner(program).run(input)
    result.result.getOrElse(throw new RuntimeException("Parsing failed: " + ErrorUtils.printParseErrors(result)))
  }
  def trace(input: String): Program = {
    val result = TracingParseRunner(program).run(input)
    result.result.getOrElse(throw new RuntimeException("Parsing failed: " + ErrorUtils.printParseErrors(result)))
  }
}

trait BVParserRules extends org.parboiled.scala.Parser {
  lazy val program: Rule1[Program] = rule {
    parenthized {
      "lambda" ~ parenthized(identifier)() ~ expression ~~> (Program(_, _))
    }()
  }
  lazy val expression: Rule1[Expr] = rule {
    ("0" ~ &(separator) ~ push(Zero) |
      "1" ~ &(separator) ~ push(One) |
      identifier ~~> Ident |
      parenthized {
        "if0" ~ whitespace ~ expression ~ expression ~ expression ~~> If0 |
          fold |
          op1 ~ whitespace ~ expression ~~> UnaryOpApply |
          op2 ~ whitespace ~ expression ~ expression ~~> BinOpApply
      }(false)) ~ optWhitespace
  }

  lazy val fold: Rule1[Fold] = rule {
    "fold" ~ whitespace ~ expression ~ expression ~
      parenthized {
        "lambda" ~ parenthized(identifier ~ optWhitespace ~ identifier)() ~ expression
      }() ~~> Fold
  }

  lazy val op1: Rule1[UnaryOp] = rule {
    "not" ~ push(Not) |
      "shl1" ~ push(Shl1) |
      "shr16" ~ push(Shr16) |
      "shr1" ~ push(Shr1) |
      "shr4" ~ push(Shr4)

  }
  lazy val op2: Rule1[BinaryOp] = rule {
    "and" ~ push(And) |
      "or" ~ push(Or) |
      "xor" ~ push(Xor) |
      "plus" ~ push(Plus)
  }

  lazy val separator = rule { whitespace | ")" | "(" }
  lazy val identifier: Rule1[String] = rule {
    group(alpha ~ zeroOrMore(alphaNumeric)) ~> identity ~ &(separator)
  }

  lazy val alpha = rule { "a" - "z" }
  lazy val alphaNumeric = rule { ("a" - "z") | "_" | ("0" - "9") }

  /** parboiled is missing a bit of abstraction so we need a bit of boilerplate here */
  def parenthized[T](r: Rule1[T])(ws: Boolean = true): Rule1[T] = optWhitespace ~ "(" ~ optWhitespace ~ r ~ optWhitespace ~ ")" ~ maybeWhitespace(ws)
  def parenthized[T1, T2](r: Rule2[T1, T2])(ws: Boolean = true): Rule2[T1, T2] = optWhitespace ~ "(" ~ optWhitespace ~ r ~ optWhitespace ~ ")" ~ maybeWhitespace(ws)
  def parenthized[T1, T2, T3](r: Rule3[T1, T2, T3])(ws: Boolean = true): Rule3[T1, T2, T3] = optWhitespace ~ "(" ~ optWhitespace ~ r ~ optWhitespace ~ ")" ~ maybeWhitespace(ws)

  def maybeWhitespace(ws: Boolean): Rule0 = if (ws) optWhitespace else EMPTY

  lazy val whitespace: Rule0 = rule {
    oneOrMore(whitespaceChars)
  }
  lazy val optWhitespace: Rule0 = rule {
    zeroOrMore(whitespaceChars)
  }
  lazy val whitespaceChars = rule {
    anyOf(" ")
  }
}
