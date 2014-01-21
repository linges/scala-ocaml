package scalaocaml

/**
  * A simple definition consists in a lowercase identifier, 
  * possibly preceded by one or several type parameters, 
  * and followed by an optional type equation, then an optional type representation, 
  * and then a constraint clause. The identifier is the name of the type constructor being defined.
  */
case class TypeDef(tyvars: List[TypeParameter], constr: String, t: Option[Type] = None,
trep: Option[TypeRepresentation], constraints: List[TypeConstraint] = List()) extends TopLevel

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
case class TRecord(m: RecordField*) extends TypeRepresentation
abstract class RecordField
case class MutableRecordField(s: String,e: PolyType) extends RecordField
case class ImmutableRecordField(s: String,e: PolyType) extends RecordField


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
case class TypeConstr(v : String, args: Type*) extends Type

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
case class ExactVariantType(ts: TagSpec*) extends PolymorphicVariantType
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
  * { ' ident }+ .  typexpr  
  */
case class PolymorphType(pvars: List[String], t: Type) extends PolyType 
