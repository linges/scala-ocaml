package scalaocaml

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.language.implicitConversions

/**
  * Type definitions are introduced by the type keyword, 
  * and consist in one or several simple definitions, 
  * possibly mutually recursive, separated by the and keyword. 
  * Each simple definition defines one type constructor.
  */
case class TypeDefinition(td: TypeDef*) extends Definition with Specification

/**
  * A simple definition consists in a lowercase identifier, 
  * possibly preceded by one or several type parameters, 
  * and followed by an optional type equation, then an optional type representation, 
  * and then a constraint clause. The identifier is the name of the type constructor being defined.
  * 
  * The optional type equation = typexpr makes the defined type equivalent 
  * to the type expression typexpr: one can be substituted for the other during typing. 
  * If no type equation is given, a new type is generated: 
  * the defined type is incompatible with any other type.
  * 
  * The optional type representation describes the data structure representing the defined type, 
  * by giving the list of associated constructors (if it is a variant type) or associated fields 
  * (if it is a record type). If no type representation is given, 
  * nothing is assumed on the structure of the type besides what is 
  * stated in the optional type equation.
  */
case class TypeDef(tyvars: List[TypeParameter], constr: String, t: Option[Type] = None,
trep: Option[TypeRepresentation] = None, constraints: List[TypeConstraint] = List()) 

/**
  * The optional type parameters are either one type variable ' ident, 
  * for type constructors with one parameter, or a list of type variables ('ident1,…,' identn), 
  * for type constructors with several parameters. 
  * Each type parameter may be prefixed by a variance constraint + (resp. -) 
  * indicating that the parameter is covariant (resp. contravariant)
  */
case class TypeParameter(id: String, variance: Option[Variance] = None)
abstract class Variance
case object Covariant extends Variance
case object Contravariant extends Variance

case class TypeConstraint(id: String, t: Type) 

abstract class TypeRepresentation 
/**
  * The type representation = { field-decl  { ; field-decl }  [;] } 
  * describes a record type. The field declarations field-decl1, …,  field-decln 
  * describe the fields associated to this record type. 
  */
case class TRecord(m: RecordField*) extends TypeRepresentation
abstract class RecordField
/**
  * The field declaration mutable field-name :  poly-typexpr behaves similarly; 
  * in addition, it allows physical modification of this field
  */
case class MutableRecordField(s: String,e: PolyType) extends RecordField
/**
  * The field declaration field-name :  poly-typexpr declares field-name 
  * as a field whose argument has type poly-typexpr. 
  */
case class ImmutableRecordField(s: String,e: PolyType) extends RecordField

/**
  * The type representation = [|] constr-decl  { | constr-decl } describes a variant type. 
  * The constructor declarations constr-decl1, …,  constr-decln describe the constructors associated 
  * to this variant type.
  */
case class ConstrDeclarations(v: ConstrDecl*) extends TypeRepresentation
case class ConstrDecl(s: String, ts: Type*)


abstract class Type extends PolyType with TagSpec with TagSpecFull

/**
  * The type expression ' ident stands for the type variable named ident. 
  * The type expression _ stands for an anonymous type variable. 
  * In data type definitions, type variables are names for the data type parameters. 
  * In type constraints, they represent unspecified types 
  * that can be instantiated by any type to satisfy the type constraint.
  */ 
case class TypeIdent(n: String) extends Type
case object TUnderscore extends Type 

/**
  * The type expression typexpr1 ->  typexpr2 denotes the type of functions mapping 
  * arguments of type typexpr1 to results of type typexpr2.
  * 
  * label-name :  typexpr1 ->  typexpr2 denotes the same function type, 
  * but the argument is labeled label.
  * 
  * ? label-name :  typexpr1 ->  typexpr2 denotes the type of functions mapping
  *  an optional labeled argument of type typexpr1 to results of type typexpr2. 
  * That is, the physical type of the function will be typexpr1 option ->  typexpr2.
  */ 
case class FunctionType(t1: Type, t2: Type) extends Type
case class LabeledFunctionType(label: String, t1: Type, t2: Type, optional: Boolean = false) extends Type 
/**
  * The type expression typexpr1 * … *  typexprn denotes the type of tuples 
  * whose elements belong to types typexpr1, …  typexprn respectively.
  */
case class TupleType(ts: Type*) extends Type
/**
  * Type constructors with no parameter, as in typeconstr, are type expressions.
  * 
  * The type expression typexpr typeconstr, where typeconstr is a
  * type constructor with one parameter, denotes the application of 
  * the unary type constructor typeconstr to the type typexpr.
  * 
  * The type expression (typexpr1,…, typexprn)  typeconstr, where typeconstr is a type constructor 
  * with n parameters, denotes the application of the n-ary type constructor 
  * typeconstr to the types typexpr1 through typexprn.
  */
case class TypeConstr(v : ExtendedName, args: Type*) extends Type

/**
  * The type expression typexpr as 'ident denotes the same type as typexpr, 
  * and also binds the type variable ident to type typexpr both in typexpr and in other types. 
  * In general the scope of an alias is the same as for a named type variable, 
  * and covers the whole enclosing definition. 
  * If the type variable ident actually occurs in typexpr, a recursive type is created. 
  * Recursive types for which there exists a recursive path that does not contain an object 
  * or polymorphic variant type constructor are rejected, except when the -rectypes mode is selected.
  * 
  * If ' ident denotes an explicit polymorphic variable, 
  * and typexpr denotes either an object or polymorphic variant type, 
  * the row variable of typexpr is captured by ' ident, and quantified upon.
  */
case class TypeAlias(t: Type, as: String) extends Type 

/**
  * The type # class-path is a special kind of abbreviation. 
  * This abbreviation unifies with the type of any object belonging to a subclass of class class-path. 
  * It is handled in a special way as it usually hides a type variable 
  * (an ellipsis, representing the methods that may be added in a subclass). 
  * In particular, it vanishes when the ellipsis gets instantiated. 
  * Each type expression # class-path defines a new type variable, 
  * so type # class-path -> #  class-path is usually not the same as 
  * type (# class-path as '  ident) -> '  ident.
  * 
  * Use of #-types to abbreviate polymorphic variant types is deprecated. 
  * If t is an exact variant type then #t translates to [< t], and #t[> `tag1 …` tagk] 
  * translates to [< t > `tag1 …` tagk]
  */
case class HashType(n: Name, ts: Type*) extends Type

/**
  * An object type < [method-type  { ; method-type }] > is a record of method types.
  * 
  * Each method may have an explicit polymorphic type: { ' ident }+ .  typexpr. 
  * Explicit polymorphic variables have a local scope, 
  * and an explicit polymorphic type can only be unified to an equivalent one, 
  * where only the order and names of polymorphic variables may change.
  * 
  * The type < {method-type ;} .. > is the type of an object whose method names 
  * and types are described by method-type1, …,  method-typen, 
  * and possibly some other methods represented by the ellipsis. 
  * This ellipsis actually is a special kind of type variable 
  * (called row variable in the literature) that stands for any number of extra method types.
  */
case class ObjectType(m: List[(String, PolyType)], withEllipsis : Boolean = false) extends Type 

/**
  * Polymorphic variant types describe the values a polymorphic variant may take.
  */
abstract class PolymorphicVariantType extends Type
/**
  * The first case is an exact variant type: all possible tags are known, 
  * with their associated types, and they can all be present. Its structure is fully known.
  */
case class ExactVariantType(ty: Option[Type], ts: TagSpec*) extends PolymorphicVariantType
/**
  * The second case is an open variant type, describing a polymorphic variant value: 
  * it gives the list of all tags the value could take, with their associated types. 
  * This type is still compatible with a variant type containing more tags. 
  * A special case is the unknown type, which does not define any tag, 
  * and is compatible with any variant type.
  */
case class OpenVariantType(ts: TagSpec*) extends PolymorphicVariantType
/**
  * The third case is a closed variant type. 
  * It gives information about all the possible tags and their associated types, 
  * and which tags are known to potentially appear in values. 
  * The exact variant type (first case) is just an abbreviation 
  * for a closed variant type where all possible tags are also potentially present.
  */
case class CloseVariantType(ts: List[TagSpecFull], tags: Option[List[String]] = None) extends PolymorphicVariantType
/**
  * In all three cases, tags may be either specified directly in the `tag-name  [of typexpr] form, 
  * or indirectly through a type expression, which must expand to an exact variant type, 
  * whose tag specifications are inserted in its place.
  */
trait TagSpec
case class TagType(name: String, t: Option[Type] = None) extends TagSpec
/**
  * Full specifications of variant tags are only used for non-exact closed types. 
  * They can be understood as a conjunctive type for the argument: 
  * it is intended to have all the types enumerated in the specification.
  */
trait TagSpecFull
case class TagTypeFull(name: String, t: Option[List[Type]] = None) extends TagSpecFull

trait PolyType 
/**
  * { ' ident }+ . typexpr  
  */
case class PolymorphType(pvars: List[String], t: Type) extends PolyType 

/**
  * Exception definitions add new constructors to the built-in
  * variant type exn of exception values. The constructors are declared 
  * as for a definition of a variant type.
  * 
  * The form exception constr-name  [of typexpr  {* typexpr}] generates a new exception, 
  * distinct from all other exceptions in the system. 
  * The form exception constr-name =  constr gives an alternate 
  * name to an existing exception. 
  */
abstract class OException extends Definition
case class NewException(name: String, ts: Type*) extends OException
case class AlternateNameException(name : String, constr: Name) extends OException

trait TypePrettyPrinter {
  self: OCamlPrettyPrinter =>

  implicit def showType(e: Type) : Doc = e match {
    case TypeConstr(v)         => v
    case TypeConstr(v, t)      => t <+> v
    case TypeConstr(v, ts @ _*)  => list(ts.toList, "", showType) <+> v
    case TypeIdent(v) => "'" <> value(v)
    case TUnderscore => "_"
    case FunctionType(t1,t2) => parens(t1 <+> "->" <+> t2)
    case LabeledFunctionType(l, t1, t2, false) =>  parens(value(l) <> ":" <+> t1 <+> "->" <+> t2)
    case LabeledFunctionType(l, t1, t2, true) =>  parens("?" <> value(l) <> ":" <+> t1 <+> "->" <+> t2)
    case TupleType(ts@ _*) =>   parens(catList(ts.map(showType), " * "))
    case TypeAlias(t, as) => parens(t <+> "as '" <> value(as))
    case HashType(n) => parens("#" <+> n)
    case HashType(n, t) => parens(t <+> "#" <+> n)
    case HashType(n, ts@ _*) => parens(list(ts.toList, "", showType) <+> "#" <+> n)
    case ObjectType(Nil, b) => enclose("<", (if(b) ".." else ""), ">")
    case ObjectType(m, b) => enclose("<", catList(m.map{case (s,t) => s <+> ":" <+> t}, semi)
        <> (if(b) " ;.." else ""), ">")
    case ExactVariantType(None, ts@ _*) => brackets(catList(ts.map(showTagSpec), " |")) 
    case ExactVariantType(Some(t), ts@ _*) => brackets(t <+> "|" <+> catList(ts.map(showTagSpec), " |")) 
    case OpenVariantType(ts@ _*) => brackets(">" <> catList(ts.map(showTagSpec), " |")) 
    case CloseVariantType(ts, None) => brackets("<" <> catList(ts.map(showTagSpecFull), " |")) 
    case CloseVariantType(ts, Some(ls)) => brackets("<" <> catList(ts.map(showTagSpecFull), " |")
      <+> ">" <+> catList(ls.map{case s => "`" <> value(s)}, ""))
  }

  implicit def showPolyType(p : PolyType) : Doc = p match {
    case PolymorphType(ps, t) =>  catList(ps.map(s => "'" <> s), "") <+> dot <+> t 
    case t: Type => showType(t)
  }

  implicit def showTagSpec(t: TagSpec) : Doc = t match {
    case TagType(n, None) => "`" <> value(n)
    case TagType(n, Some(t)) => "`" <> value(n) <+> "of" <+> t
    case t : Type => showType(t)
  }

  implicit def showTagSpecFull(t: TagSpecFull) : Doc = t match {
    case TagTypeFull(n, None) => "`" <> value(n)
    case TagTypeFull(n, Some(t)) => "`" <> value(n) <+> "of" <+> catList(t.map(showType), " &")
    case t : Type => showType(t)
  }

  implicit def showTypeDef(e: TypeDef) : Doc = e match {
    case TypeDef(tvars, constr, t, tr, cs) => {
      showTypeParameters(tvars) <+> constr <>
      t.map(" =" <+> showType(_)).getOrElse("") <>
      tr.map(" =" <+> showTypeRepresentation(_)).getOrElse("") <+>
      lsep(cs.map(showTypeConstraint), "")
    }
  }
 
  def showTypeConstraint(cs: TypeConstraint) : Doc = cs match {
    case TypeConstraint(id, t) => "constraint '" <> id <+> "=" <+> t 
  }

  def showTypeRepresentation(tr: TypeRepresentation) : Doc = tr match {
    case TRecord(m@ _*)  => enclose("{",  catList(m.map(showRecordField), semi)
        ,"}")
    case ConstrDeclarations(l@ _*) => "|" <+> lsep(l.map(showConstrDecl), line <> "|" )
  }

  def showConstrDecl(cd: ConstrDecl) : Doc = cd match {
    case ConstrDecl(s) => value(s)
    case ConstrDecl(s, ts@ _*) => value(s) <+> "of" <+> catList(ts.map(showType), " *")
  }

  implicit def showRecordField(rf: RecordField) : Doc = rf match {
    case ImmutableRecordField(s, t) => s <+> ":" <+> t
    case MutableRecordField(s, t) => "mutable" <+> s <+> ":" <+> t
  }

  def showTypeParameters(ts: List[TypeParameter]) : Doc = 
    if(ts.isEmpty) ""
    else if(ts.size == 1) showTypeParameter(ts(0))
    else list(ts.toList, "", showTypeParameter)

  def showTypeParameter(tp: TypeParameter) : Doc = tp match {
    case TypeParameter(id, Some(Covariant)) => "+" <> "'" <> id
    case TypeParameter(id, Some(Contravariant)) => "-" <> "'" <> id
    case TypeParameter(id, None) => "'" <> id
  }

  def showException(e: OException) : Doc = e match {
    case NewException(name) => "exception" <+> name
    case NewException(name, ts@ _*) => "exception" <+> name <+> "of" <+> catList(ts.map(showType), " *")
    case AlternateNameException(name , constr) => "exception" <+> name <+> "=" <+> constr
  }
}

trait TypeParser extends RegexParsers with Parsers {
  self: OCamlParser =>

  lazy val tparentheses: Parser[Type] = "(" ~> typeexpr <~ ")"

  lazy val typeconstr : Parser[Type] = 
    ("(" ~> (typeexpr <~ ",") ~ repsep(typeexpr, ",") <~ ")") ~ extendedname ^^
          { case t~ts~n => TypeConstr(n, (t::ts):_*)} 
  def singleargconstr(t: Type) : Parser[Type] = 
    ( extendedname ^^ { case n => TypeConstr(n, t)} into singleargconstr) | success(t)

  lazy val simpletypeconstr = 
    extendedname ^^ { case n => TypeConstr(n)} 

  def tupletype(t:Type) = "*" ~> rep1sep(lvlt3, "*") ^^ { case ts => TupleType(t::ts:_*) } |
                          success(t)

  def astype(t:Type) = "as" ~> "'" ~> ident ^^ { case i => TypeAlias(t,i) } | success(t)

  lazy val simpletypes = simpletypeconstr| typeconstr | simplehashtype | tident | objecttype |
                         polymorphicvarianttype


  lazy val tident = "'" ~> ident ^^ {case i => TypeIdent(i) }

  lazy val simplehashtype = "#" ~> name ^^ { case n => HashType(n) }

  lazy val hashtype :Parser[Type] = 
    ("(" ~> (typeexpr <~ ",") ~ repsep(typeexpr, ",") <~ ")") ~ ("#" ~> name) ^^
          { case t~ts~n => HashType(n, (t::ts):_*)} 

  def singlearghash(t: Type) :Parser[Type] = 
    ("#" ~> name ^^ { case n => HashType(n, t) } into singlearghash) |  success(t)

  def functiontype(t: Type) :Parser[Type] = 
    "->" ~> typeexpr ^^ { case t2 => FunctionType(t,t2) } | success(t)

  lazy val labeledfunctiontype : Parser[Type] = 
    ("?" ~> lowercaseident <~ ":") ~ (lvlt3 <~ "->") ~ typeexpr ^^
       { case i~t~t2 => LabeledFunctionType(i, t, t2, true) } |
    (lowercaseident <~ ":") ~ (lvlt3 <~ "->") ~ typeexpr ^^
       { case i~t~t2 => LabeledFunctionType(i, t, t2, false) }

  lazy val polytype  =
       rep1("'" ~> ident) ~ "." ~ typeexpr ^^ { case ls~_~t => PolymorphType(ls,t) } |
       typeexpr

  lazy val methodtype = lowercaseident ~ ":" ~ polytype ^^ { case l~_~p => (l,p) }

  lazy val typeexpr : Parser[Type] = lvlt0

  lazy val objecttype = "<" ~> ( 
    rep1sep(methodtype, ";") ~ (";"~".."| ";").? ^^
     { case mts~el => 
       el match {
         case Some(";"~"..") => ObjectType(mts, true)
         case _ => ObjectType(mts, false)
       }
     } |
    ".." ^^ { case _ => ObjectType(Nil, true) } |
     success(ObjectType(Nil, false))
  )  <~ ">"

  lazy val polymorphicvarianttype = "[" ~>(
        ">" ~> repsep(tagspec, "|") ^^ { case ts => OpenVariantType(ts:_*) } |
        ("<" ~ ("|".?)) ~> repsep(tagspecfull, "|") ~ (">" ~> rep1("`" ~> capitalizedident)).? ^^
           { case ts~tn => CloseVariantType(ts, tn) } |
         (typeexpr <~ "|").? ~ rep1sep(tagspec, "|") ^^ { case t~ts => ExactVariantType(t, ts:_*) }
    ) <~ "]"

  lazy val tagtype = "`" ~> capitalizedident ~ ( "of" ~> typeexpr ).? ^^
                     { case i~t => TagType(i, t) }

  lazy val tagspec = tagtype | typeexpr
  lazy val tagspecfirst = tagspec

  lazy val tagspecfull = "`" ~> capitalizedident ~ (("of" ~ ("&").?) ~> rep1sep(typeexpr, "&") ).? ^^
                         { case i~ts => TagTypeFull(i, ts) } | typeexpr

  lazy val lvlt0 = lvlt1 into astype // as
  lazy val lvlt1 = (labeledfunctiontype | lvlt2) into functiontype// ->
  lazy val lvlt2 = lvlt3 into tupletype //*
  lazy val lvlt3 = (hashtype | lvlt4) into singlearghash // #
  lazy val lvlt4 = lvlt5 into singleargconstr // constr
  lazy val lvlt5 = simpletypes | tparentheses 



  lazy val typedefinition = ("type" ~> rep1sep(typedef, "and") ) ^^
                             { case l => TypeDefinition(l:_*) }
// case class TypeDef(tyvars: List[TypeParameter], constr: String, t: Option[Type] = None,
// trep: Option[TypeRepresentation], constraints: List[TypeConstraint] = List()) 

  lazy val typedef = typeparams ~ lowercaseident ~ ("=" ~> typeexpr).? ~ typerepresentation.? ~
  rep(typeconstraint) ^^
  { case tp~n~t~tr~cs => TypeDef(tp, n, t, tr, cs) }

  lazy val typerepresentation = trecord | constrdeclarations 

  lazy val trecord = "=" ~ "{" ~> (rep1sep(recordfield, ";") ^^
    {case fs => TRecord(fs:_*)}) <~ (";".?) <~ "}"

  lazy val recordfield = 
    "mutable" ~> lowercaseident ~ ":" ~ polytype ^^ { case l~_~pt => MutableRecordField(l, pt) } |
    lowercaseident ~ ":" ~ polytype ^^ { case l~_~pt => ImmutableRecordField(l, pt) }

  lazy val constrdeclarations = 
    ("=" ~ ("|".?)) ~> rep1sep(constrdecl, "|") ^^ { case cd => ConstrDeclarations(cd:_*) }

  lazy val constrdecl = (capitalizedident | "()") ~ "of" ~ rep1sep(typeexpr, "*") ^^
                        { case n~_~ts => ConstrDecl(n, ts:_*) } |
                        (capitalizedident | "()") ^^ { case n => ConstrDecl(n) } 

  lazy val typeconstraint = "constraint" ~> "'" ~> ident ~ "=" ~ typeexpr ^^ 
    { case i~_~t => TypeConstraint(i,t) }

  lazy val typeparams = "(" ~> rep1sep(typeparam, ",") <~ ")" | 
     typeparam ^^ { List(_) } | success(Nil)

  lazy val typeparam = (contravariant|covariant).? ~ "'" ~ ident ^^
    { case v~_~i => TypeParameter(i, v) }

  lazy val covariant = "+" ^^^ { Covariant }
  lazy val contravariant = "-" ^^^ { Contravariant } 

  lazy val exceptiondefinition =
    "exception" ~> capitalizedident ~ ("of" ~> rep1sep(typeexpr, "*")) ^^
       { case i~ts => NewException(i, ts:_*) } |
    "exception" ~> capitalizedident ~ "=" ~ constrpath ^^
       { case i~_~c => AlternateNameException(i,c) } |
    "exception" ~> capitalizedident  ^^
       { case i => NewException(i) } 

}
