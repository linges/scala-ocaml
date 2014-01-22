package scalaocaml

trait Pattern extends Parameter
/**
  * A pattern that consists in a value name matches any value, binding the name to the value. 
  * The pattern _ also matches any value, but does not bind any name.
  */
case class PVar(v: String) extends Pattern
case object Underscore extends Pattern

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
  * The pattern constr _ matches all variants whose constructor is constr. //TODO test
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
case class PRecord(m: Map[Name, Pattern]) extends Pattern
case class RecordPunning(names: Name*) extends Pattern

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
case class PTypeconstr(name: Name) extends Pattern

trait PatternMatching 
case class MatchingWithGuard(p: Pattern, g: Expr, e: Expr) extends PatternMatching
case class Matching(p: Pattern, e: Expr) extends PatternMatching

trait Parameter 
case class LabeledPar(label: String, t:Option[Type] = None) extends Parameter
case class LabeledParWithPattern(label: String, p: Pattern) extends Parameter
case class OptionalLabeledPar(label: String, p: Option[Pattern] = None, t: Option[Type] = None, default : Option[Expr] = None) extends Parameter
