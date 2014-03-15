package scalaocaml

/**
  * Module types are the module-level equivalent of type expressions: 
  * they specify the general shape and type properties of modules.
  */
sealed abstract class ModuleType


/**
  * The expression modtype-path is equivalent to the module type bound to the name modtype-path.
  */
case class MTVar(name : ExtendedName) extends ModuleType

/**
  * Signatures are type specifications for structures. 
  * Signatures sig … end are collections of type specifications 
  * for value names, type names, exceptions, module names and module type names. 
  */
case class Signatur(e: Specification*) extends ModuleType

/**
  * The module type expression functor ( module-name :  module-type1 ) ->  module-type2 
  * is the type of functors (functions from modules to modules) that take as argument
  * a module of type module-type1 and return as result a module of type module-type2. 
  */
case class FunctorType(name: String, as: ModuleType, mt: ModuleType) extends ModuleType

/**
  * Assuming module-type denotes a signature, the expression module-type with  mod-constraint { and mod-constraint } 
  * denotes the same signature where type equations have been added 
  * to some of the type specifications, as described by the constraints following the with keyword.
  */
case class MTWith(mt: ModuleType, cs: MTConstraint*) extends ModuleType

abstract class MTConstraint
/**
  * The constraint type [type-parameters]  typeconstr =  typexpr adds the 
  * type equation = typexpr to the specification of the type component named typeconstr of the constrained signature. 
  */
case class MTTypeConstraint( tps : List[TypeParameter], typeConstr: ExtendedName, t: Type) extends MTConstraint

/**
  * The constraint module module-path =  extended-module-path adds type equations to all type components
  * of the sub-structure denoted by module-path, making them equivalent to the 
  * corresponding type components of the structure denoted by extended-module-path.
  */
case class MTModuleConstraint(m1 : Name, m2: ExtendedModulePath) extends MTConstraint

//TODO external specification is missing
trait Specification
/**
  * A specification of a value component in a signature is written 
  * val value-name :  typexpr, where value-name is the name of the value and typexpr its expected type.
  */
case class SVal(name : String, t : Type) extends Specification

/**
  * The specification exception constr-decl in a signature requires the matching structure to provide 
  * an exception with the name and arguments specified in the definition, 
  * and makes the exception available to all users of the structure.
  */
case class SException(cd: ConstrDecl) extends Specification

/**
  * A specification of a module component in a signature is written
  * module module-name :  module-type, where module-name is the name 
  * of the module component and module-type its expected type.
  * 
  * For specifying a module component that is a functor, one may write
  * module module-name (  name1 :  module-type1 ) … (  namen :  module-typen ) :  module-type
  * instead of
  * module module-name : functor (  name1 :  module-type1 ) -> … ->  module-type 
  */
case class ModuleSpecification(name : String, fs: List[(String, ModuleType)], mt : ModuleType) extends Specification

/**
  * A module type component of a signature can be specified either 
  * as a manifest module type or as an abstract module type.
  */
case class ModuleTypeSpecification(name : String, impl: Option[ModuleType] = None) extends Specification

/**
  * The expression include module-type in a signature performs 
  * textual inclusion of the components of the signature denoted by module-type. 
  */
case class Include(mt: ModuleType) extends Specification 

/**
  * The expression open module-path in a signature does not specify any components. 
  * It simply affects the parsing of the following items of the signature, 
  * allowing components of the module denoted by module-path to be referred to by their 
  * simple names name instead of path accesses module-path . name. 
  * The scope of the open stops at the end of the signature expression.
  */
case class Open(name: Name) extends Specification with Definition

/**
  * Module expressions are the module-level equivalent of value expressions: 
  * they evaluate to modules, thus providing implementations for 
  * the specifications expressed in module types.
  */
sealed abstract class ModuleExpr

/**
  * The expression module-path evaluates to the module bound to the name module-path.
  */
case class MVar(name : Name) extends ModuleExpr

/**
  * Structures struct … end are collections of definitions for value names, 
  * type names, exceptions, module names and module type names. 
  */
case class Struct(mi: ModuleItem*) extends ModuleExpr

/**
  * The expression functor ( module-name :  module-type ) ->  module-expr evaluates 
  * to a functor that takes as argument modules of the type module-type1, 
  * binds module-name to these modules, evaluates module-expr in the extended environment, 
  * and returns the resulting modules as results. 
  */
case class Functor(name: String, mt: ModuleType, me: ModuleExpr) extends ModuleExpr

/**
  * The expression module-expr1 (  module-expr2 ) evaluates module-expr1 
  * to a functor and module-expr2 to a module, and applies the former to the latter. 
  */
case class FunctorApp(me1: ModuleExpr, me2: ModuleExpr) extends ModuleExpr

/**
  * The expression ( module-expr :  module-type ) checks that the type of module-expr is a 
  * subtype of module-type, that is, that all components specified in module-type are 
  * implemented in module-expr, and their implementation meets the requirements given in module-type.
  */
case class MEAscription(me: ModuleExpr, mt: ModuleType) extends ModuleExpr
trait ModuleItem

trait Definition extends ModuleItem with TopLevelPhrase
/**
  * A definition for a module type is written module type modtype-name =  module-type. 
  * It binds the name modtype-name to the module type denoted by the expression module-type.
  */
case class ModuleTypeDef(name : String, impl: ModuleType) extends Definition

/**
  * The basic form for defining a module component is module module-name =  module-expr, 
  * which evaluates module-expr and binds the result to the name module-name.
  * One can write
  * module module-name :  module-type =  module-expr
  * 
  * instead of
  * module module-name = (  module-expr :  module-type ).
  * 
  * Another derived form is
  * module module-name (  name1 :  module-type1 ) … (  namen :  module-typen ) =  module-expr
  * 
  * which is equivalent to
  * module module-name = functor (  name1 :  module-type1 ) -> … ->  module-expr 
  */
case class ModuleDefinition(name: String, ce: ModuleExpr, fs: List[(String, ModuleType)] = List(), mt: Option[ModuleType] = None) extends Definition

/**
  * The expression include module-expr in a structure re-exports in the current structure 
  * all definitions of the structure denoted by module-expr. 
  */
case class IncludeDef(me: ModuleExpr) extends Definition 

case class External(name: String, t: Type, externaldec: String) extends Definition

trait ModulePrettyPrinter {
  self: OCamlPrettyPrinter =>

  implicit def showModuleExpr(me: ModuleExpr) : Doc = me match {
    case MVar(n) => n
    case Struct(mi@ _*) =>"struct" <> nest(lsep(mi.map(showModuleItem), ";;")) <@> "end"
    case Functor(name, mt, me) => "functor" <+> parens( name <+> ":" <+> mt) <+> "->" <+> me
    case FunctorApp(me1, me2) => me1 <+> parens( me2 )
    case MEAscription(me, mt) => parens( me <+> ":" <+> mt)
  }

  def showModuleItem(i : ModuleItem) : Doc = i match {
    case i: Expr => showExpr(i)
    case i: Definition => showDefinition(i)
  }

  def showSpecification(s: Specification) : Doc = s match {
    case cs: ClassSpecification => showClassSpecification(cs)
    case ctd: ClassTypeDefinition => showClassTypeDefinition(ctd)
    case SVal(name, t) => "val" <+> name <+> ":" <+> t
    case SException(cd) => "exception" <+> showConstrDecl(cd)
    case ModuleSpecification(name,Nil, mt) => "module" <+> name <+> ":" <+> mt
    case ModuleSpecification(name, fs, mt) => "module" <+> name <> 
        catList(fs.map{ case (s,mt) => parens(s <+> ":" <+> mt)}, "") <+> ":" <+> mt
    case ModuleTypeSpecification(name, impl) => "module type" <+> name <> 
      impl.map(" =" <+> _).getOrElse("")
    case Include(mt) => "include" <+> mt
    case d: Definition => showDefinition(d)
  }

  def showDefinition(e: Definition) : Doc = e match {
    case External(n,t,c) => "external" <+> n <+> ":" <+> t <+> "=" <+> "\"" <> c <> "\""
    case IncludeDef(me) => "include" <+> me
    case Open(name) => "open" <+> name
    case ctd: ClassTypeDefinition => showClassTypeDefinition(ctd)
    case cd: ClassDefinition => showClassDefinition(cd)
    case e: OException => showException(e)
    case Let(v@ _*) => group("let" <+> lsep(v.map(showLetBinding), line <>"and"))
    case LetRec(v@ _*) => group("let rec" <+> lsep(v.map(showLetBinding), line <>"and"))
    case TypeDefinition(v@ _*) => group("type" <+> lsep(v.map(showTypeDef), line <>"and"))
    case ModuleTypeDef(name, impl) => "module type" <+> name <+> "=" <+> impl
    case ModuleDefinition(name, me, Nil, mt) => "module" <+> name <> mt.map(" :" <+> _).getOrElse("") <+> "=" <+> me
    case ModuleDefinition(name, me, fs, mt) => "module" <+> name <>  
        catList(fs.map{ case (s,mt) => parens(s <+> ":" <+> mt)}, "") <+>
        mt.map(":" <+> _).getOrElse("") <+> "=" <+> me
  }


  implicit def showModuleType(mt: ModuleType) : Doc = mt match {
    case MTVar(n) => n
    case Signatur(s@ _*) => "sig" <> nest(lsep(s.map(showSpecification), ";;")) <@> "end"
    case FunctorType(name, as, mt) => "functor" <+> parens( name <+> ":" <+> as ) <+> "->" <+> mt
    case MTWith(mt, cs@ _*) => mt <+> "with" <+> catList(cs.map(showMTConstraint), " and")
  }

  def showMTConstraint(c : MTConstraint) : Doc = c match {
    case MTTypeConstraint(tps, name, t) => "type" <+> showTypeParameters(tps) <+> name <+> "=" <+> t
    case MTModuleConstraint(m1, m2) => "module" <+> m1 <+> "=" <+> m2
  }

}



