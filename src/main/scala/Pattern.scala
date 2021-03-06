package scalaocaml

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.language.implicitConversions

trait Pattern extends Parameter
/**
  * A pattern that consists in a value name matches any value, binding the name to the value. 
  * The pattern _ also matches any value, but does not bind any name.
  */
case class PVar(v: String) extends Pattern

/**
  * The pattern pattern1 as  value-name matches the same values as pattern1. 
  * If the matching against pattern1 is successful, the name value-name is bound to the matched value, 
  * in addition to the bindings performed by the matching against pattern1.
  */
case class Alias(p: Pattern, name: String) extends Pattern 

/**
  * A type constraint can appear in a parenthesized pattern, as in ( pattern1 :  typexpr ).
  * This constraint forces the type of pattern1 to be compatible with typexpr.
  */
case class PAscription(p: Pattern, t: Type) extends Pattern 

/**
  * The pattern pattern1 |  pattern2 represents the logical “or” of the two patterns pattern1 and pattern2. 
  * A value matches pattern1 |  pattern2 if it matches pattern1 or pattern2. 
  * The two sub-patterns pattern1 and pattern2 must bind
  * exactly the same identifiers to values having the same types.
  */
case class OrPattern(a: Pattern, b: Pattern) extends Pattern
/**
  * The pattern constr (  pattern1 , … ,  patternn ) matches all variants whose constructor is equal to constr, 
  * and whose arguments match pattern1 …  patternn. 
  * It is a type error if n is not the number of arguments expected by the constructor.
  * The pattern constr _ matches all variants whose constructor is constr.
  */
case class ConstrPattern(constr: Name, p: Pattern*) extends Pattern

/**
  * The pattern `tag-name  pattern1 matches all polymorphic variants
  * whose tag is equal to tag-name, and whose argument matches pattern1.
  */
case class PolyVariantPattern(tagname: String, p: Pattern) extends Pattern

/**
  * The pattern pattern1 ::  pattern2 matches non-empty lists whose heads match pattern1, 
  * and whose tails match pattern2.
  * The pattern [ pattern1 ; … ;  patternn ] matches lists of length n whose elements match
  * pattern1 …patternn, respectively. This pattern behaves like pattern1 :: … ::  patternn :: [].
  */
case class PList(l: Pattern*) extends Pattern
case class FixSizeList(l: List[Pattern]) extends Pattern

/**
  * The pattern pattern1 , … ,  patternn matches n-tuples
  * whose components match the patterns pattern1 through patternn. 
  */
case class PTuple(l: Pattern*) extends Pattern 

/**
  * The pattern { field1 =  pattern1 ; … ;  fieldn =  patternn } matches records that define 
  * at least the fields field1 through fieldn, and such that the value associated to fieldi matches the pattern patterni,
  * for i = 1,… , n. The record value can define more fields than field1 …fieldn; 
  * the values associated to these extra fields are not taken into account for matching.
  */
case class PRecord(m: Map[Name, Option[Pattern]]) extends Pattern

/**
  * The pattern [| pattern1 ; … ;  patternn |] matches arrays of length n 
  * such that the i-th array element matches the pattern patterni, for i = 1,… , n.
  */
case class ArrayPattern(ps: Pattern*) extends Pattern 

/**
  * If the type [('a,'b,…)] typeconstr = [ ` tag-name1  typexpr1 | … | ` tag-namen  typexprn] is defined,
  * then the pattern #typeconstr is a shorthand for the following or-pattern:
  * ( `tag-name1(_ :  typexpr1) | … | ` tag-namen(_ :  typexprn)). 
  * It matches all values of type [< typeconstr ].
  */
case class PTypeconstr(name: ExtendedName) extends Pattern

trait PatternMatching 
case class MatchingWithGuard(p: Pattern, g: Expr, e: Expr) extends PatternMatching
case class Matching(p: Pattern, e: Expr) extends PatternMatching

trait Parameter 
case class LabeledPar(label: String, t:Option[Type] = None) extends Parameter
case class LabeledParWithPattern(label: String, p: Pattern) extends Parameter
case class OptionalLabeledPar(label: String, p: Option[Pattern] = None, t: Option[Type] = None, default : Option[Expr] = None) extends Parameter


trait PatternPrettyPrinter {
  self : OCamlPrettyPrinter =>

  implicit def showParameter(p: Parameter) : Doc = p match {
    case p: Pattern => showPattern(p)
    case LabeledPar(l, None) => "~" <> l
    case LabeledPar(l, Some(t)) => parens("~" <> l <+> ":" <+> t)
    case LabeledParWithPattern(l, p) => "~" <> l <+> ":" <+> p
    case OptionalLabeledPar(l, None, None, None) => "?" <> l
    case OptionalLabeledPar(l, Some(p), None, None) => "?" <> l <+> ":" <+> p
    case OptionalLabeledPar(l, None, t, e) => "?" <> parens(l <> 
        t.map(" :"<+> _).getOrElse("") <> e.map(" ="<+> _).getOrElse("") )
    case OptionalLabeledPar(l, Some(p), t, e) => "?" <> l <+> ":" <+> parens(p <> 
        t.map(" :"<+> _).getOrElse("") <> e.map(" ="<+> _).getOrElse("") )
  }

  implicit def showPatternMatching(b: PatternMatching) : Doc = b match {
    case Matching(p,e) => "|"<+> p <+> "->" <+> e
    case MatchingWithGuard(p,g,e) => "|"<+> p <+>"when" <+> g <+> "->" <+> e
  }

  implicit def showPattern(e: Pattern) : Doc = e match {
    case c: Constant => showConstant(c)
    case PVar(v) => value(v)
    case PTuple(l@ _*) => list(l.toList, "", showPattern)
    case PList(l@ _*) => if (l.isEmpty) "[]" else list(l.toList, "", showPattern, space<>"::")
    case PRecord(m) => enclose("{",  catList(m.map{
      case (s,t) => s <>
       t.map(space <> "=" <+> _).getOrElse(empty)
     }.toList, semi)
        ,"}")
    case ConstrPattern(n) => n
    case ConstrPattern(n, p@ _*) => n <> parens(catList(p.map(showPattern),comma))
    case OrPattern(a,b) => parens(a <+> "|" <+> b)
    case FixSizeList(l) => brackets( catList(l.map(showPattern), semi) )
    case ArrayPattern(l@ _*)         => enclose("[|", catList(l.map(showPattern), semi), "|]")
    case Alias(p,n) => p <+> "as" <+> n
    case PTypeconstr(n) => "#" <> n
    case PolyVariantPattern(n, p) => "`" <> n <+> p
  }
}

trait PatternParser extends RegexParsers with Parsers {
  self: OCamlParser =>

  def simplepattern: Parser[Pattern] = polyvariantpattern | constant |  pvar | arrayp | listp |
                                       recordp | typeconstrp | constrp 
  def pattern: Parser[Pattern] = lvlp0 
  def pvar : Parser[Pattern] = lowercaseident ^^ { PVar(_) }

  lazy val constrp = constrpath ^^ { case f => ConstrPattern(f)} 
  lazy val arrayp: Parser[Pattern] = "[|" ~> rep1sep(pattern, ";") <~ (";" ?) <~ "|]" ^^
                            { case l => ArrayPattern(l:_*) }
  lazy val listp: Parser[Pattern] = "[" ~> rep1sep(pattern, ";") <~ (";" ?) <~ "]" ^^
                            { case l => FixSizeList(l) }

  def asp(p: Pattern) : Parser[Pattern] = "as" ~> valuename ^^ { case n => Alias(p,n) } | success(p)

  def orpattern(p: Pattern) : Parser[Pattern] = ("|" ~> lvlp2 ^^ { OrPattern(p,_) } into orpattern) | success(p)

  def listop(p: Pattern) : Parser[Pattern] =  "::" ~> repsep(lvlp4, "::") ^^ { case b => PList((p::b):_*) } | success(p)

  def tuplep (p: Pattern) : Parser[Pattern] = "," ~> repsep(lvlp3, ",") ^^ { case b => PTuple((p::b):_*) } | success(p)

  lazy val parenthesesp = "(" ~> pattern <~ ")"

  lazy val constrapp = constrpath ~ lvlp5 ^^ { case f ~ l => ConstrPattern(f, l)} 

  lazy val recordp = "{" ~> rep1sep(recordFieldp, ";") <~ "}" ^^
                              { case l => PRecord(l.toMap) }

  lazy val recordFieldp: Parser[(Name, Option[Pattern])] = name ~ (("=" ~> pattern)?) ^^
                                                             { case n~e => (n,e) }

  lazy val typeconstrp = "#" ~> extendedname ^^ { case n => PTypeconstr(n) } 
  lazy val polyvariantpattern = "`" ~> capitalizedident ~  pattern ^^ { case n~p => PolyVariantPattern(n, p) } 

  //We use this do represent priorities.
  //Higher level means higher priority.
  lazy val lvlp0 = lvlp1 into asp
  lazy val lvlp1 = lvlp2 into orpattern
  lazy val lvlp2 = lvlp3 into tuplep
  lazy val lvlp3 = lvlp4 into listop
  lazy val lvlp4 = constrapp  | lvlp5
  lazy val lvlp5 = simplepattern | parenthesesp


  lazy val patternmatching :Parser[List[PatternMatching]] = firstmatching ~
        rep(matching|matchingwithguard) ^^ { case pm~pms => pm::pms}


  lazy val firstmatching = ("|".? ~> pattern)  ~ ("->" ~> expr) ^^ { case p~e => Matching(p,e) } |
                           ("|".? ~> pattern) ~ ("when" ~> expr) ~ ("->" ~> expr) ^^
                               { case p~e~g => MatchingWithGuard(p,e,g) }

  lazy val matching = ("|" ~> pattern)  ~ ("->" ~> expr) ^^ { case p~e => Matching(p,e) } 

  lazy val matchingwithguard = ("|" ~> pattern) ~ ("when" ~> expr) ~ ("->" ~> expr) ^^
                               { case p~e~g => MatchingWithGuard(p,e,g) }

  lazy val parameter : Parser[Parameter] = pattern |  
    labeledparameterwithpattern | labeledparameter | optionallabeledpar

  lazy val labeledparameter = "~" ~>  labelname ^^ { case l => LabeledPar(l) } |
    (("(" ~> "~" ~>  labelname ~ (":" ~> typeexpr).?) <~ ")") ^^ 
                               { case l~t => LabeledPar(l,t) }
  lazy val labeledparameterwithpattern = "~" ~>  labelname ~ ( ":" ~> pattern)  ^^ 
                               { case l~p => LabeledParWithPattern(l,p) }

  lazy val optionallabeledpar = 
    "?" ~>  (labelname <~ ":" <~ "(") ~ pattern ~ ((":" ~> typeexpr)?) ~ (("=" ~> expr)?) <~")" ^^
         { case l~p~t~e => OptionalLabeledPar(l,Some(p),t,e) } |
    "?" ~>  labelname ~ ((":" ~> pattern)?) ^^ { case l~p => OptionalLabeledPar(l,p) } |
    "?" ~> "(" ~>  labelname  ~ ((":" ~> typeexpr)?) ~ (("=" ~> expr)?) <~")" ^^
         { case l~t~e => OptionalLabeledPar(l,None,t,e) } 
}
