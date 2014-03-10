package scalaocaml

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.language.implicitConversions

/**
  * The syntactic class of constants comprises literals from the four base types
  * (integers, floating-point numbers, characters, character strings), 
  * and constant constructors from both normal and polymorphic variants, 
  * as well as the special constants false, true, (), [], and [||], 
  * which behave like constant constructors, and begin end, which is equivalent to (). 
  * A pattern consisting in a constant matches the values that are equal to this constant.
  */
sealed abstract class Constant extends Expr with Pattern 
case class OInt(n: Int) extends Constant
case class OFloat(n: Double) extends Constant
sealed abstract class OBoolean extends Constant
case object True extends OBoolean
case object False extends OBoolean
case class OChar(c: Char) extends Constant 
case class OString(s: String) extends Constant
case object Unit extends Constant
case object EmptyList extends Constant 
case object EmptyArray extends Constant 
case object EmptyBeginEnd extends Constant
case class TagName(s: String) extends Constant 

trait ConstantPrettyPrinter {
  self: OCamlPrettyPrinter =>

  implicit def showConstant(c: Constant) : Doc = c match {
    case OInt(v)       => value(v)
    case OFloat(v)     => value(v)
    case OChar(c)      => "'" <> value(c) <> "'"
    case OString(s)    => '"' <> string(s) <> '"'
    case TagName(s)    => "`" <> s
    case True          => "true"
    case False         => "false"
    case Unit          => "()"
    case EmptyList     => "[]"
    case EmptyArray    => "[||]"
    case EmptyBeginEnd => "begin end"
  }
}

trait ConstantParser extends RegexParsers with Parsers {
  self: OCamlParser =>

  lazy val constant: Parser[Constant] = char | string | float | num  | unit | otrue | ofalse |
                                   emptylist | emptyarray | emptybeginend | tagname

  lazy val char: Parser[OChar] = """\'.\'""".r ^^ {  s: String => OChar(s.apply(1)) }
  lazy val string: Parser[OString] = """"(\\"|[^"])*"""".r ^^ { s: String => OString(s.substring(1, s.length() - 1)) }
  lazy val num: Parser[OInt] = """-?\d+""".r ^^ { case d => OInt(d.toInt) }
  //TODO underscore :  For convenience and readability, underscore characters (_) are accepted (and ignored) within floating-point literals.
  lazy val float: Parser[OFloat] = 
    """-?(\d+\.\d*|\.\d+)([Ee](-|\+)?\d+)?""".r ^^ {
     d => OFloat(d.toDouble)
  }

  lazy val unit: Parser[Unit.type] = """\(\)""".r ^^ { _ => Unit } //TODO
  lazy val otrue: Parser[True.type] = """true""".r ^^ { _ => True }
  lazy val ofalse: Parser[False.type] = """False""".r ^^ { _ => False }
  lazy val emptylist: Parser[EmptyList.type] = """\[\]""".r ^^ { _ => EmptyList }
  lazy val emptyarray: Parser[EmptyArray.type] = """\[\|\|\]""".r ^^ { _ => EmptyArray }
  lazy val emptybeginend: Parser[EmptyBeginEnd.type] = """begin end""".r ^^ { _ => EmptyBeginEnd }
  lazy val tagname: Parser[TagName] = """`""".r ~ capitalizedident ^^ { case _ ~ s => TagName(s) }
}
