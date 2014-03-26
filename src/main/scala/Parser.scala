package scalaocaml

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.language.implicitConversions


object OCamlParser extends OCamlParser

/**
 * Parser implementation module based on scala's combinators
 */
trait OCamlParser extends RegexParsers with Parsers
      with IdentifierParser with ConstantParser with TypeParser with PatternParser with ExprParser
      with ClassParser with ModuleParser {

  protected override val whiteSpace = """(\s|(?m)\(\*(\*(?!\))|[^*])*\*\))+""".r

  //This is necessary to allow names that start with a keyword:
  //Every literal that is a keyword is convert to a regex that matches
  //the keyword and a word boundary.
  override implicit def literal(s: String): Parser[String] =
    if (keywordlist.contains(s)) 
      regex((s + """\b""").r) 
    else super.literal(s)

  def parseExpr(in: String): Either[String, Expr] = {
    try {
      parseAll(expr, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg + " (" + in1.pos.line + ":" + in1.pos.column + ")" )
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }


  def parseClassType(in: String): Either[String, ClassType] = {
    try {
      parseAll(classtype, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg + " (" + in1.pos.line + ":" + in1.pos.column + ")" )
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }

  def parseClassExpr(in: String): Either[String, ClassExpr] = {
    try {
      parseAll(classexpr, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg + " (" + in1.pos.line + ":" + in1.pos.column + ")" )
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }

  def parseDef(in: String): Either[String, Definition] = {
    try {
      parseAll(definition, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg + " (" + in1.pos.line + ":" + in1.pos.column + ")" )
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }

  def parseType(in: String): Either[String, Type] = {
    try {
      parseAll(typeexpr, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg + " (" + in1.pos.line + ":" + in1.pos.column + ")" )
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }



  def parseIdentifier(in: String): Either[String, Identifier] = {
    try {
      parseAll(identifier, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg + " (" + in1.pos.line + ":" + in1.pos.column + ")" )
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
          Left(msg + " (" + in1.pos.line + ":" + in1.pos.column + ")" )
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }


  def parseModuleExpr(in: String): Either[String, ModuleExpr] = {
    try {
      parseAll(moduleexpr, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg + " (" + in1.pos.line + ":" + in1.pos.column + ")" )
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }

  def parseModuleType(in: String): Either[String, ModuleType] = {
    try {
      parseAll(moduletype, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg + " (" + in1.pos.line + ":" + in1.pos.column + ")" )
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }

  def parseUnitImplementation(in: String): Either[String, UnitImplementation] = {
    try {
      parseAll(unitimplementation, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg + " (" + in1.pos.line + ":" + in1.pos.column + ")" )
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }

  def parseUnitInterface(in: String): Either[String, UnitInterface] = {
    try {
      parseAll(unitinterface, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, in1) =>
          Left(msg + " (" + in1.pos.line + ":" + in1.pos.column + ")" )
      }
    } catch {
      //should no longer happen
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString)
    }
  }
  
  def all : Parser[Any] = moduletype |  definition | pattern | typeexpr | classtype 
}

