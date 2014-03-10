package scalaocaml

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.language.implicitConversions


abstract class Identifier
case class ExtendedModuleName(moduleName: String, paths : List[ExtendedModulePath] = List()) extends Identifier
case class ExtendedModulePath(name: ExtendedModuleName, l: List[ExtendedModuleName] = List()) extends Identifier

case class ExtendedName(id: String, path: Option[ExtendedModulePath] = None) extends Identifier
case class Name(id: String, path: List[String]) extends Identifier
object Name
{
  def apply(s: String) : Name =
    {
      val path = s.split("\\.")
      Name(path.last, path.dropRight(1).toList)
    }
}




trait IdentifierPrettyPrinter {
  self : OCamlPrettyPrinter =>

  implicit def showIdentifier(n: Identifier) : Doc = n match {
    case Name(n, Nil) => n
    case Name(n, l) => catList(l.map(string),dot) <> dot <> n
    case ExtendedName(n, None) => n
    case ExtendedName(n, Some(l)) => l <> dot <+> n
    case ExtendedModulePath(n, Nil) => n
    case ExtendedModulePath(n, l) => catList(l.map(showIdentifier),dot) <> dot <> n
    case ExtendedModuleName(n, Nil) => n
    case ExtendedModuleName(n, l) => n <+> catList(l.map(x => parens(showIdentifier(x))), "")
  }
}


trait IdentifierParser extends RegexParsers with Parsers {
  self: OCamlParser =>

  lazy val name: Parser[Name] = (rep1sep(capitalizedident, ".") <~ ".").? ~ lowercaseident ^^
  { case path ~ s => Name(s,path.getOrElse(Nil)) }

  lazy val extendedname = ( extendedmodulepath <~ "." ).? ~ lowercaseident ^^
      { case p~n => ExtendedName(n,p) }

  lazy val extendedmodulename : Parser[ExtendedModuleName] = capitalizedident ~ 
  rep("(" ~> extendedmodulepath <~ ")") ^^
    { case n ~ es => ExtendedModuleName(n, es) }

  lazy val extendedmodulepath : Parser[ExtendedModulePath] = extendedmodulename ~ 
    rep ("." ~> extendedmodulename) ^^
  { case n ~ es => ExtendedModulePath(n, es) }

  lazy val valuepath: Parser[Name] = (rep1sep(capitalizedident, ".") <~ ".").? ~ 
   valuename ^^
  { case path ~ s => Name(s,path.getOrElse(Nil)) }

  lazy val constrpath: Parser[Name] = (rep1sep(capitalizedident, ".") <~ ".").? ~ 
  capitalizedident  ^^
  { case path ~ s => Name(s,path.getOrElse(Nil)) }

  lazy val valuename = (lowercaseident | operatorname)
  lazy val operatorname = "(" ~> (infixop | prefixsymbol) <~")" ^^ { case s => "("+s+")" }

  lazy val ident: Parser[String] =  """[a-zA-Z_][a-zA-Z0-9_']*""".r into checkKeyword
  lazy val lowercaseident: Parser[String] =  """[a-z_][a-zA-Z0-9_']*""".r into checkKeyword
  lazy val capitalizedident: Parser[String] =   """[A-Z][a-zA-Z0-9_']*""".r into checkKeyword

  lazy val labelname = """[a-z_][a-zA-Z0-9_']*""".r into checkKeyword

  lazy val keyword = 
    (keywords.map(_ + "\\b") ++
    keysymbols).mkString("", "|", "").r

  lazy val infixop =  (infixsymbol |
    """[*+=<>-]∣-\.∣∣!=∣or∣\|\|∣&∣&&|:=|mod∣land∣lor∣lxor∣lsl∣lsr∣asr|::""".r  ) into checkKeyword
  lazy val infixsymbol=  ("""[=<>@^|&+*/$%-]""" + operatorchar + "*").r 
  lazy val operatorchar = """[-!$%&*+./:<=>?@^|~]""".r

  def checkKeyword(s: String) : Parser[String] = if (s.matches(keyword.toString)) failure("keyword used as identifier: " + s) else success(s)

  lazy val prefixsymbol = (("!"  + operatorchar + "*").r  |
                             ("[?~]" +operatorchar + "+").r ) into checkKeyword
  val keywords = List(
    "and",
    "as",
    "assert",
    "begin",
    "class",
    "constraint",
    "do",
    "done",
    "downto",
    "else",
    "end",
    "exception",
    "external",
    "false",
    "for",
    "fun",
    "function",
    "functor",
    "if",
    "in",
    "include",
    "inherit",
    "inherit!",
    "initializer",
    "lazy",
    "let",
    "match",
    "method",
    "method!",
    "module",
    "mutable",
    "new",
    "object",
    "of",
    "open",
    "open!",
    "or",
    "private",
    "rec",
    "sig",
    "struct",
    "then",
    "to",
    "true",
    "try",
    "type",
    "val",
    "val!",
    "virtual",
    "when",
    "while",
    "with", 
    "parser",  
    "value")
    val keysymbols = List( "\\|", "->") 

    val ops = List(
    "!=",    "#",     "&",     "&&",    "'",
    "\\(",     "\\)",     "\\*",     "\\+",     ",",
    "-",     "-\\.",    "->",    "\\.",     "\\.\\.", 
    ":",     "::",    ":=",    ":>",    ";",
    ";;",    "<",     "<-",    "=",     ">",  
    ">\\]",    ">\\}",    "\\?",     "\\[",     "\\[<",   
    "\\[>",    "\\[\\|",    "]",     "_",     "`",   
    "\\{",     "\\{<",    "\\|",     "\\|\\]",    "\\|\\|", 
    "\\}",     "~",    
    "\\$",     "\\$\\$",    "\\$:",    "<:",    "<<", 
    ">>",    "\\?\\?"  )


}
