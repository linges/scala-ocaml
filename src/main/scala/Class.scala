package scalaocaml

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.language.implicitConversions

/**
  * Class types are the class-level equivalent of type expressions: 
  * they specify the general shape and type properties of classes.
  * 
  * The class type expression typexpr ->  class-type is the type of class functions 
  * (functions from values to classes) that take as argument a value 
  * of type typexpr and return as result a class of type class-type.
  */
case class ClassType(args: List[ClassTypeFunctionArg], body: ClassBodyType)
case class ClassTypeFunctionArg(t: Type, label: Option[String] = None, optional : Boolean = false)

abstract class ClassBodyType
/**
  * The expression classtype-path is equivalent to the class type bound to the name 
  * classtype-path. Similarly, the expression [ typexpr1 , …  typexprn ]  classtype-path 
  * is equivalent to the parametric class type bound to the name classtype-path, 
  * in which type parameters have been instantiated to respectively typexpr1, …typexprn.
  */
case class SimpleClassBodyType(targs: List[Type], classPath: Name) extends ClassBodyType

/**
  * The class type expression object [( typexpr )]  {class-field-spec} end is the type 
  * of a class body. It specifies its instance variables and methods. 
  * In this type, typexpr is matched against the self type, 
  * therefore providing a name for the self type.
  */
case class NormalClassBodyType(t : Option[Type], cfs: ClassFieldSpec*) extends ClassBodyType 

abstract class ClassFieldSpec
/**
  * The inheritance construct inherit class-body-type provides for inclusion 
  * of methods and instance variables from other class types. 
  * The instance variable and method types from class-body-type are added 
  * into the current class type.
  */
case class InheritSpec(c: ClassType) extends ClassFieldSpec
/**
  * A specification of an instance variable is written 
  * val [mutable] [virtual] inst-var-name : typexpr, 
  * where inst-var-name is the name of the instance variable 
  * and typexpr its expected type. The flag mutable indicates whether 
  * this instance variable can be physically modified. 
  * The flag virtual indicates that this instance variable is not initialized. 
  * It can be initialized later through inheritance.
  */
case class ValSpec(name : String, t: Type, mutable : Boolean = false, virtual : Boolean = false) extends ClassFieldSpec 
/**
  * The specification of a method is written method [private] method-name : poly-typexpr, 
  * where method-name is the name of the method and poly-typexpr its expected type, 
  * possibly polymorphic. The flag private indicates that the method 
  * cannot be accessed from outside the object.
  * A virtual method specification is written method 
  * [private] virtual method-name : poly-typexpr, 
  * where method-name is the name of the method and poly-typexpr its expected type.
  */
case class MethodSpec(name: String, pt: PolyType, privatee: Boolean = false, virtual: Boolean = false) extends ClassFieldSpec
/**
  * The construct constraint typexpr1 =  typexpr2 forces 
  * the two type expressions to be equal. This is typically used to 
  * specify type parameters: in this way, they can be bound to specific type expressions.
  */
case class ClassConstraint(t1: Type, t2: Type) extends ClassFieldSpec


/**
  * Class expressions are the class-level equivalent of value expressions: 
  * they evaluate to classes, thus providing implementations 
  * for the specifications expressed in class types.
  */
sealed abstract class ClassExpr

/**
  * The expression class-path evaluates to the class bound to the name class-path. 
  * Similarly, the expression [ typexpr1 , …  typexprn ]  class-path evaluates 
  * to the parametric class bound to the name class-path, 
  * in which type parameters have been instantiated respectively to typexpr1, …typexprn.
  */
case class SimpleClassExpr(targs: List[Type], classPath: Name) extends ClassExpr

/**
  * The expression ( class-expr :  class-type ) checks that class-type 
  * matches the type of class-expr (that is, that the implementation class-expr 
  * meets the type specification class-type). The whole expression evaluates to 
  * the same class as class-expr, except that all components not specified 
  * in class-type are hidden and can no longer be accessed.
  */
case class ClassAscription(ce: ClassExpr, ct: ClassType) extends ClassExpr
/**
  * Class application is denoted by juxtaposition of 
  * (possibly labeled) expressions. It denotes the class whose
  * constructor is the first expression applied to the given arguments.
  */
case class ClassApp(ce: ClassExpr, a: Argument, as: Argument*) extends ClassExpr
/**
  * The expression fun [[?]label-name:] pattern -> class-expr evaluates 
  * to a function from values to classes.
  */
case class ClassFun(pars: List[Parameter], e: ClassExpr) extends ClassExpr
/**
  * The let and let rec constructs bind value names locally, 
  * as for the core language expressions.
  * 
  * If a local definition occurs at the very beginning of a class definition, 
  * it will be evaluated when the class is created 
  * (just as if the definition was outside of the class). 
  * Otherwise, it will be evaluated when the object constructor is called.
  */
case class ClassLetIn(lb: List[LetBinding], e: ClassExpr) extends ClassExpr
case class ClassLetRecIn(lb: List[LetBinding], e: ClassExpr) extends ClassExpr

/**
  * The expression object class-body end denotes a class body. 
  * This is the prototype for an object : 
  * it lists the instance variables and methods of an objet of this class.
  */
case class ClassObject(cb: ClassBody) extends ClassExpr

/**
  * In a class body, the pattern ( pattern  [: typexpr] ) is matched against self, 
  * therefore providing a binding for self and self type. 
  * Self can only be used in method and initializers.
  */
case class ClassBody(cs: List[ClassField], p: Option[Pattern] = None, t : Option[Type] = None) 
abstract class ClassField

/**
  * The inheritance construct inherit class-expr allows reusing methods 
  * and instance variables from other classes. The class expression class-expr 
  * must evaluate to a class body. The instance variables, methods and initializers 
  * from this class body are added into the current class. 
  * The addition of a method will override any previously defined method of the same name.
  */
case class Inherit(ce: ClassExpr, as: Option[String] = None) extends ClassField 

/**
  * The definition val [mutable] inst-var-name = expr adds an instance variable 
  * inst-var-name whose initial value is the value of expression expr. 
  * The flag mutable allows physical modification of this variable by methods.
  */
case class Val(name : String, e : Expr, mutable : Boolean = false, t: Option[Type] = None) extends ClassField   
/**
  * A variable specification is written val [mutable] virtual inst-var-name : typexpr. 
  * It specifies whether the variable is modifiable, and gives its type.
  */
case class VirtualVal(name : String, t : Type, mutable : Boolean = false) extends ClassField
case class Initializer(e: Expr) extends ClassField 
case class Constraint (t1: Type, t2: Type) extends ClassField 

/**
  * A method definition is written method method-name =  expr. 
  * The definition of a method overrides any previous definition of this method. 
  * The method will be public (that is, not private) if any of the definition states so.
  *
  * A private method, method private method-name =  expr, 
  * is a method that can only be invoked on self 
  * (from other methods of the same object, 
  * defined in this class or one of its subclasses).
  */
case class Method(name: String, pars: List[Parameter], e: Expr, t: Option[Type] = None, privatee : Boolean = false) extends ClassField 

/**
  * Methods may have an explicitly polymorphic type, allowing them to be 
  * used polymorphically in programs (even for the same object). 
  * By giving an explicit polymorphic type in the method definition, 
  * immediately after the method name, i.e. 
  * method [private] method-name :  {' ident}+ . typexpr =  expr; 
  */
case class PolyMethod(name : String, pt : PolyType, e : Expr, privatee : Boolean = false)  extends ClassField

/**
  * A method specification is written 
  * method [private] virtual method-name : poly-typexpr. 
  * It specifies whether the method is public or private, 
  * and gives its type. If the method is intended to be polymorphic, 
  * the type must be explicitly polymorphic.
  */
case class VirtualMethod(name : String, pt: PolyType, privatee : Boolean = false)  extends ClassField  

/**
  * This is the counterpart in signatures of class definitions. 
  * A class specification matches a class definition if they have the 
  * same type parameters and their types match.
  */
case class ClassSpecification(c: ClassSpec, cs: ClassSpec*) extends Specification
case class ClassSpec(name: String, ct: ClassType, virtual : Boolean = false, typeParams: List[String] = List())

/**
  * A class type definition class class-name =  class-body-type defines an 
  * abbreviation class-name for the class body type class-body-type. 
  * As for class definitions, two type abbreviations class-name and # class-name are also defined. 
  * The definition can be parameterized by some type parameters. 
  * If any method in the class type body is virtual, the definition must be flagged virtual.
  */
case class ClassTypeDefinition(cd: ClassTypeDef, cds: ClassTypeDef*) extends Specification with Definition
case class ClassTypeDef(name: String, ct: ClassBodyType, virtual : Boolean = false, typeParams: List[String] = List())

/**
  * A class definition class class-binding  { and class-binding } is recursive. 
  * Each class-binding defines a class-name that can be used in the whole expression 
  * except for inheritance. It can also be used for inheritance, 
  * but only in the definitions that follow its own.
  */
case class ClassDefinition(cd: ClassBinding, cbs: ClassBinding*) extends Definition
/**
  * A class binding binds the class name class-name to the value of expression class-expr. 
  * It also binds the class type class-name to the type of the class, 
  * and defines two type abbreviations : class-name and # class-name.
  */
case class ClassBinding(name : String, pars: List[Parameter], ce: ClassExpr,  virtual : Boolean = false, typeParams: List[String] = List(), ct: Option[ClassType] = None)

trait ClassPrettyPrinter {
  self : OCamlPrettyPrinter =>


  def showClassDefinition(cs: ClassDefinition) : Doc = cs match {
    case ClassDefinition(c) => "class" <+> c 
    case ClassDefinition(c, cs@ _*) => "class" <+> c <+> "and" <+> catList(cs.map(showClassBinding), " and") 
  }

  implicit def showClassBinding(cs: ClassBinding) : Doc = cs match {
    case ClassBinding(name, pars, ce, virt, Nil, ct) =>
     (if(virt) "virtual " else "") <>
      name <+> catList(pars.map(showParameter), "") <>
      ct.map(" :" <+> _).getOrElse("") <+> "=" <+> ce
    case ClassBinding(name, pars, ce, virt, tp, ct) =>
     (if(virt) "virtual " else "") <>
      brackets(catList(tp.map("'" <> _), "")) <+>
      name <+> catList(pars.map(showParameter), "") <>
      ct.map(" :" <+> _).getOrElse("") <+> "=" <+> ce
  }
  def showClassTypeDefinition(cs: ClassTypeDefinition) : Doc = cs match {
    case ClassTypeDefinition(c) => "class type" <+> c 
    case ClassTypeDefinition(c, cs@ _*) => "class type" <+> c <+> "and" <+> catList(cs.map(showClassTypeDef), " and") 
  }

  implicit def showClassTypeDef(cs: ClassTypeDef) : Doc = cs match {
    case ClassTypeDef(name, ct, virt, Nil) =>
     (if(virt) "virtual " else "") <> name <+> "=" <+> ct 
    case ClassTypeDef(name, ct, virt, tp) =>
     (if(virt) "virtual " else "") <>
      brackets(catList(tp.map("'" <> _), "")) <+>
      name <+> "=" <+> ct
  }

  def showClassSpecification(cs: ClassSpecification) : Doc = cs match {
    case ClassSpecification(c) => "class" <+> c 
    case ClassSpecification(c, cs@ _*) => "class" <+> c <+> "and" <+> catList(cs.map(showClassSpec), " and") 
  }

  implicit def showClassSpec(cs: ClassSpec) : Doc = cs match {
    case ClassSpec(name, ct, virt, Nil) =>
     (if(virt) "virtual " else "") <> name <+> ":" <+> ct 
    case ClassSpec(name, ct, virt, tp) =>
     (if(virt) "virtual " else "") <>
      brackets(catList(tp.map("'" <> _), "")) <+>
      name <+> ":" <+> ct
  }

  implicit def showClassExpr(ce: ClassExpr) : Doc = ce match {
    case ClassAscription(e,t)      => parens(e <+> ":" <+> t)
    case SimpleClassExpr(Nil, n) => n
    case SimpleClassExpr(targs, n) =>
      brackets(catList(targs.map(showType), comma)) <+> n
    case ClassObject(b)  => "object" <+> b <@> "end"
    case ClassLetIn(v, e) => group("let" <+> lsep(v.map(showLetBinding), line <>"and") <@> "in" <> nest(line <> e))
    case ClassLetRecIn(v, e) => group("let rec" <+> lsep(v.map(showLetBinding), line <>"and") <@> "in" <> nest(line <> e))
    case ClassApp(f,x)             => parens(f <+> x)
    case ClassApp(f,x, xs@ _*)     => parens(f <+> x <+> catList(xs.map(showArg), space))
    case ClassFun(args,e)  => parens("fun" <+> catList(args.map(showParameter), "") <+> "->" <+> e)
  }

  
  implicit def showClassBody(b: ClassBody) : Doc = b match {
    case ClassBody(cs, p, t) => (p.map{ pp =>
      parens( pp <> t.map{ tt => " :" <+> tt}.getOrElse(""))}.getOrElse(""): Doc) <> 
      nest(line <> lsep(cs.map(showClassField), line))
  }

  implicit def showClassField(cf: ClassField) : Doc = cf match {
    case Inherit(ce, as) => "inherit" <+> ce <> as.map(" as" <+> _).getOrElse("")
    case Val(name, e, muta, t) =>
      "val" <> (if(muta) " mutable" else "") <+> name <>
      t.map(" :" <+> _).getOrElse("") <+>
       "=" <+> e
    case VirtualVal(name, t, muta) =>
      "val" <> (if(muta) " mutable" else "") <+>
      "virtual" <+>
      name <+> ":" <+> t
    case Initializer(e) => "initializer" <+> e
    case Constraint(t1,t2) => "constraint" <+> t1 <+> "=" <+> t2 
    case Method(name, pars, e, t, priv) =>
      "method" <> (if(priv) " private" else "") <+>
      name <+> catList(pars.map(showParameter), "") <>
      t.map(" :" <+> _).getOrElse("") <+> 
      "=" <+> e
    case PolyMethod(name, pt, e, priv) =>
      "method" <> (if(priv) " private" else "") <+>
      name <+> ":" <+> pt <+> "=" <+> e
    case VirtualMethod(name, pt, priv) =>
      "method" <> (if(priv) " private" else "") <+>
      "virtual" <+> name <+> ":" <+> pt
  }

  implicit def showClassFieldSpec(cfs: ClassFieldSpec) : Doc = cfs match {
    case ClassConstraint(t1,t2) => "constraint" <+> t1 <+> "=" <+> t2 
    case MethodSpec(name, pt, priv, virt) =>
      "method" <> (if(priv) " private" else "") <>
      (if(virt) " virtual" else "") <+>
      name <+> ":" <+> pt
    case ValSpec(name, t, muta, virt) =>
      "val" <> (if(muta) " mutable" else "") <>
      (if(virt) " virtual" else "") <+>
      name <+> ":" <+> t
    case InheritSpec(c) => "inherit" <+> c
  }

  implicit def showClassBodyType(cbt: ClassBodyType) : Doc = cbt match {
    case SimpleClassBodyType(Nil, n) => n
    case SimpleClassBodyType(targs, n) => 
      brackets(catList(targs.map(showType), comma)) <+> n
    case NormalClassBodyType(None, cfs@ _*) =>
      "object" <@> catList(cfs.map(showClassFieldSpec), "") <@> "end"
    case NormalClassBodyType(Some(t), cfs@ _*) =>
      "object" <+> parens(t) <> nest(line <> lsep(cfs.map(showClassFieldSpec), "")) <@> "end"
  }

  implicit def showClassType(ct: ClassType) : Doc = ct match {
    case ClassType(Nil, b) => b
    case ClassType(cfs, b) => catList(cfs.map(showClassTypeFunctionArg), " ->") <+> "->" <+> b
  }
  
  implicit def showClassTypeFunctionArg(arg: ClassTypeFunctionArg) : Doc = arg match {
    case ClassTypeFunctionArg(t, label, op) =>
        (if(op) "?" else "") <> label.map(value(_) <> ": ").getOrElse("") <> t 
  }
}

trait ClassParser extends RegexParsers with Parsers {
  self: OCamlParser =>

  lazy val classtype = rep(classfunctionarg <~ "->" ) ~ (classbodytype) ^^ 
                       { case args~b => ClassType(args,b) } |
                       classbodytype ^^ { case b => ClassType(Nil, b) }


  lazy val classfunctionarg = "?" ~> (labelname <~ ":") ~
                              simpletypes ^^ { case l~t => ClassTypeFunctionArg(t, Some(l), true)} |
                              (labelname <~ ":") ~
                              typeexpr ^^ { case l~t => ClassTypeFunctionArg(t, Some(l), false)} |
                              typeexpr ^^ { ClassTypeFunctionArg(_)}

  lazy val classbodytype : Parser[ClassBodyType] = normalclassbodytype | simpleclassbodytype  

  lazy val simpleclassbodytype = ("[" ~> rep1sep(typeexpr, ",") <~ "]")  ~ name ^^
                                 {case ts~n => SimpleClassBodyType(ts,n)} |
                                 name ^^ { case n => SimpleClassBodyType(Nil, n) }

  lazy val normalclassbodytype = "object" ~> ("(" ~> typeexpr <~ ")").? ~ rep(classfieldspec) <~
                                 "end" ^^ { case t~cs => NormalClassBodyType(t, cs:_*) }

  lazy val classfieldspec = inheritspec | valspec | methodspec | classconstraint  

  lazy val inheritspec = "inherit" ~> classtype ^^ { InheritSpec(_) }

  lazy val valspec = ("val" ~> mutablevirtual) ~ lowercaseident ~ ":" ~ typeexpr ^^
                     { case m~v~n~_~t => ValSpec(n, t, m, v) }

  lazy val mutablevirtual = ("mutable" ~> success(true) | success(false)) ~
                            ("virtual" ~> success(true) | success(false)) 

  lazy val methodspec = "method" ~> ("private" ~> success(true) | success(false)) ~
                        ("virtual" ~> success(true) | success(false)) ~ lowercaseident ~
                        (":" ~> polytype) ^^ { case p~v~n~t => MethodSpec(n, t, p,  v) }

  lazy val classconstraint = "constraint" ~>  typeexpr ~ "=" ~ typeexpr ^^ 
                            { case t1~_~t2 => ClassConstraint(t1,t2) }

  lazy val inherit = "inherit" ~> classexpr ~ ("as" ~> valuename).? ^^ { case c~v => Inherit(c,v) }

  lazy val oval = "val" ~> ("mutable" ~> success(true) | success(false)) ~ lowercaseident ~
                  (":" ~> typeexpr).? ~ ( "=" ~> expr) ^^ { case m~n~t~e => Val(n,e,m,t) }

  lazy val virtualval = "val" ~> ("mutable" ~> success(true) | success(false)) ~ lowercaseident ~
                  (":" ~> typeexpr) ^^ { case m~n~t => VirtualVal(n,t,m) }

  lazy val initializer = "initializer" ~> expr ^^ { case e => Initializer(e) }
  lazy val constraint = "constraint" ~> typeexpr ~ "=" ~ typeexpr ^^ { case t1~_~t2 => Constraint(t1,t2) }

  lazy val classmethod = "method" ~> ("private" ~> success(true) | success(false)) ~
                         lowercaseident ~ rep(parameter) ~ (":" ~> typeexpr).? ~
                         ("=" ~> expr) ^^ { case p~n~ps~t~e => Method(n,ps,e,t,p)}

  lazy val polymethod = "method" ~> ("private" ~> success(true) | success(false)) ~
                         lowercaseident ~ (":" ~> polytype) ~
                         ("=" ~> expr) ^^ { case p~n~t~e => PolyMethod(n,t,e,p)}

  lazy val virtualmethod = "method" ~> ("private" ~> success(true) | success(false)) ~
                         ("virtual" ~> lowercaseident) ~ (":" ~> polytype) ^^ 
                         { case p~n~t => VirtualMethod(n,t,p)}

  lazy val classfield = inherit | oval | virtualval | initializer | constraint | classmethod |
                        polymethod | virtualmethod

  lazy val classexpr : Parser[ClassExpr] =
    ( simpleclassexpr | classascription | classfun | classletin | classletrecin |
      classobject | cparentheses ) into classapp

  lazy val cparentheses = "(" ~> classexpr <~ ")"

  lazy val simpleclassexpr = ("[" ~> rep1sep(typeexpr, ",") <~ "]")  ~ name ^^
                             {case ts~n => SimpleClassExpr(ts,n)} |
                             name ^^ { case n => SimpleClassExpr(Nil, n) }

  lazy val classascription = "(" ~> classexpr ~ ":" ~ classtype <~ ")" ^^ 
                            { case e~_~t => ClassAscription(e,t) }

  def classapp(e:ClassExpr) : Parser[ClassExpr] = (arg ^^ { case a => ClassApp(e,a) } into classapp) |
                                             success(e)

  lazy val classfun = "fun" ~> rep1(parameter) ~ "->" ~ classexpr ^^
                  { case ps~_~ce => ClassFun(ps, ce) }

  lazy val classletin: Parser[ClassLetIn] = ("let" ~> rep1sep(letbinding, "and") <~ "in") ~ classexpr ^^
                             { case l~e => ClassLetIn(l,e) }

  lazy val classletrecin: Parser[ClassLetRecIn] = ("let" ~> "rec" ~> rep1sep(letbinding, "and") <~
                             "in") ~ classexpr ^^
                             { case l~e => ClassLetRecIn(l,e) }

  lazy val classobject = "object" ~> classbody <~ "end" ^^ { ClassObject(_) }

  lazy val classbody = ("(" ~> pattern ~ (":" ~> typeexpr).? <~ ")")~ rep(classfield) ^^
                       { case p~t~cf => ClassBody(cf, Some(p), t) } |
                       rep(classfield) ^^ { ClassBody(_) }

  lazy val classdefinition = ("class" ~> rep1sep(classbinding, "and") ) ^^
                             { case l => ClassDefinition(l.head, l.tail:_*) }

  lazy val classbinding = ("virtual" ~> success(true) | success(false)) ~
                          (("[" ~> rep1("'" ~> ident) <~ "]") | success(Nil)) ~
                          lowercaseident ~ rep(parameter) ~ (":" ~> classtype).? ~
                          ("=" ~> classexpr) ^^
                          { case v~tps~n~ps~ct~ce => ClassBinding(n, ps, ce, v, tps, ct) }

  lazy val classspecification = ("class" ~> rep1sep(classspec, "and") ) ^^
                             { case l => ClassSpecification(l.head, l.tail:_*) }

  lazy val classspec  = ("virtual" ~> success(true) | success(false)) ~
                          (("[" ~> rep1("'" ~> ident) <~ "]") | success(Nil)) ~
                          lowercaseident ~ (":" ~> classtype) ^^ 
                          { case v~tps~n~ct => ClassSpec(n, ct, v, tps) }

  lazy val classtypedefinition = ("class" ~> "type" ~> rep1sep(classtypedef, "and") ) ^^
                             { case l => ClassTypeDefinition(l.head, l.tail:_*) }


  lazy val classtypedef  = ("virtual" ~> success(true) | success(false)) ~
                          (("[" ~> rep1("'" ~> ident) <~ "]") | success(Nil)) ~
                          lowercaseident ~ ("=" ~> classbodytype) ^^ 
                          { case v~tps~n~ct => ClassTypeDef(n, ct, v, tps) }
}
