package scalaocaml

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.language.implicitConversions


abstract class Identifier
case class ExtendedModuleName(moduleName: String, paths : List[ExtendedModulePath] = List()) extends Identifier
case class ExtendedModulePath(l: List[ExtendedModuleName]) extends Identifier

case class ExtendedName(id: String, path: Option[ExtendedModulePath] = None) extends Identifier
case class Name(id: String, path: List[String]) extends Identifier with DirectiveArgument
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
    case Name(n, l) => l.map(string).reduceLeft( (a,z) => a <> dot <> z) <> dot <> n
    case ExtendedName(n, None) => n
    case ExtendedName(n, Some(l)) => l <> dot <> n
    case ExtendedModulePath(l) => l.map(showIdentifier).reduceLeft( (a,z) => a <> dot <> z)
    case ExtendedModuleName(n, Nil) => n
    case ExtendedModuleName(n, l) => n <+> catList(l.map(x => parens(showIdentifier(x))), "")
  }
}

trait IdentifierParser extends RegexParsers with Parsers {
  self: OCamlParser =>

  lazy val identifier = name | extendedname | valuepath | constrpath

  lazy val name: Parser[Name] = (rep1sep(capitalizedident, ".") <~ ".").? ~ lowercaseident ^^
  { case path ~ s => Name(s,path.getOrElse(Nil)) }

  lazy val modtypepath = ( extendedmodulepath <~ "." ).? ~ ident ^^
      { case p~n => ExtendedName(n,p) }

  lazy val extendedname = ( extendedmodulepath <~ "." ).? ~ lowercaseident ^^
      { case p~n => ExtendedName(n,p) }

  lazy val extendedmodulename : Parser[ExtendedModuleName] = capitalizedident ~ 
  rep("(" ~> extendedmodulepath <~ ")") ^^
    { case n ~ es => ExtendedModuleName(n, es) }

  lazy val extendedmodulepath : Parser[ExtendedModulePath] = rep1sep (extendedmodulename, ".") ^^
        { case es => ExtendedModulePath(es) }

  lazy val valuepath: Parser[Name] = ((rep1sep(capitalizedident, ".") <~ ".").?) ~ 
   valuename ^^
  { case path ~ s => Name(s,path.getOrElse(Nil)) }

  lazy val constrpath = capitalname

  lazy val capitalname: Parser[Name] = rep1sep(capitalizedident, ".") <~ not(".") ^^
    { case path=> Name(path.last , path.dropRight(1)) }

  lazy val valuename = (lowercaseident | operatorname)
  lazy val operatorname = "(" ~> (infixop | prefixsymbol) <~")" ^^ { case s => "("+s+")" }

  lazy val ident: Parser[String] =  """[a-zA-Z_][a-zA-Z0-9_']*""".r into checkKeyword
  lazy val lowercaseident: Parser[String] =  """[a-z_][a-zA-Z0-9_']*""".r into checkKeyword
  lazy val capitalizedident: Parser[String] =   """[A-Z][a-zA-Z0-9_']*""".r into checkKeyword

  lazy val labelname = """[a-z_][a-zA-Z0-9_']*""".r into checkKeyword

  lazy val keywords = 
    (keywordlist.map(_ + "\\b") ++
    keysymbols).mkString("", "|", "").r

  lazy val infixop =  (infixsymbol |
    """[*+=<>-]∣-\.∣∣!=∣or∣\|\|∣&∣&&|:=|mod∣land∣lor∣lxor∣lsl∣lsr∣asr|::""".r  ) into checkKeyword
  lazy val infixsymbol=  ("""[=<>@^|&+*/$%-]""" + operatorchar + "*").r 
  lazy val operatorchar = """[-!$%&*+./:<=>?@^|~]""".r

  def checkKeyword(s: String) : Parser[String] = if (s.matches(keywords.toString)) failure("keyword used as identifier: " + s) else success(s)

  lazy val prefixsymbol = (("!"  + operatorchar + "*").r  |
                             ("[?~]" +operatorchar + "+").r ) into checkKeyword
  val keywordlist = List(
    "and",
    "as",
    //"assert",
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
   // "lazy",
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
