package scalaocaml

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.language.implicitConversions

  object HelloWorld {
    def main(args: Array[String]) {
      val a = OCamlParser.parseExpr(args(0))
      println(a)
      println("|".matches("\\|"))
    }
  }


object OCamlParser extends OCamlParser

/**
 * Parser implementation module based on scala's combinators
 */
trait OCamlParser extends RegexParsers with Parsers
      with IdentifierParser with ConstantParser with PatternParser with ExprParser{
  def parseExpr(in: String): Either[String, Expr] = {
    try {
      parseAll(expr, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg)
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }
  def parseAny(in: String): Either[String, Any] = {
    try {
      parseAll(all, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg)
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }
  def definition : Parser[Definition] = let | letrec
  def all : Parser[Any] = expr | definition | pattern
}

