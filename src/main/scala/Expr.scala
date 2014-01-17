package scalaocaml

trait Expr

/**
  * The syntactic class of constants comprises literals from the four base types
  * (integers, floating-point numbers, characters, character strings), 
  * and constant constructors from both normal and polymorphic variants, 
  * as well as the special constants false, true, (), [], and [||], 
  * which behave like constant constructors, and begin end, which is equivalent to (). 
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

/**
  * An expression consisting in an access path evaluates
  * to the value bound to this path in the current evaluation environment.
  * The path can be either a value name or an access path to a value component of a module.
  */
case class Var(v: Name) extends Expr

/**
  * Function application is denoted by juxtaposition of (possibly labeled) expressions.
  * The expression expr  argument1 …  argumentn evaluates the expression expr and 
  * those appearing in argument1 to argumentn.
  */
case class App(f: Expr, x: Expr, xs : Expr*) extends Expr


/**
  * {{{
  * function pattern1 -> expr1
  * | … 
  * |       patternn  -> exprn
  * }}}
  * This expression evaluates to a functional value with one argument.
  * When this function is applied to a value v,
  * this value is matched against each pattern pattern1 to patternn.
  */
case class Function(l: PatternMatching*) extends Expr

/**
  * {{{fun parameter1 …  parametern ->  expr}}}
  * This expression is equivalent to:
  * {{{fun parameter1 -> … fun  parametern ->  expr}}}
  */
case class Fun(args: List[Parameter], e: Expr, guard : Option[Expr] = None) extends Expr 
/**
  * The let and let rec constructs bind value names locally. The construct
  * {{{let pattern1 =  expr1 and … and  patternn = exprn in expr}}}
  * evaluates expr1 …  exprn in some unspecified order and
  * matches their values against the patterns pattern1 …  patternn.
  * An alternate syntax is provided to bind variables to functional values: 
  * instead of writing
  * {{{let ident = fun  parameter1 …  parameterm ->  expr}}}
  * in a let expression, one may instead write
  * {{{let ident  parameter1 …  parameterm =  expr }}}
  */
case class LetIn(lb: List[LetBinding], e: Expr) extends Expr
case class LetRecIn(lb: List[LetBinding], e: Expr) extends Expr


//Control Structures
/**
  * The expression expr1 ; expr2 evaluates expr1 first,
  * then expr2, and returns the value of expr2.
  */
case class Sequence(l: Expr*) extends Expr

/**
  * The expression if expr1 then  expr2 else  expr3 evaluates 
  * to the value of expr2 if expr1 evaluates to the boolean true,
  * and to the value of expr3 if expr1 evaluates to the boolean false.
  * The else expr3 part can be omitted, in which case it defaults to else ().
  */
case class IfThenElse(condition: Expr, thenE: Expr, elseE: Expr) extends Expr 
case class IfThen(condition: Expr, thenE: Expr) extends Expr 

/**
  * The expression
  * {{{
  * match expr 
  * with pattern1 -> expr1 
  * |	… 
  * |    patternn -> exprn
  * }}} 
  * matches the value of expr against the patterns pattern1 to patternn. 
  * If the matching against patterni succeeds, the associated expression expri is evaluated,
  * and its value becomes the value of the whole match expression. 
  */
case class Match(e: Expr, l: PatternMatching*) extends Expr

/**
  * The expression while expr1 do  expr2 done repeatedly
  * evaluates expr2 while expr1 evaluates to true.
  */
case class While(c: Expr, body: Expr) extends Expr 

/**
  * The expression for name =  expr1 to  expr2 do  expr3 done first evaluates 
  * the expressions expr1 and expr2 (the boundaries) into integer values n and p.
  * Then, the loop body expr3 is repeatedly evaluated in an environment where name
  * is successively bound to the values n, n+1, …, p−1, p. 
  * The loop body is never evaluated if n > p.
  */
case class ForTo(c: String, from: Expr, to: Expr, e: Expr) extends Expr

/**
  * The expression for name =  expr1 downto  expr2 do  expr3 done evaluates similarly, 
  * except that name is successively bound to the values n, n−1, …, p+1, p. 
  * The loop body is never evaluated if n < p.
  */
case class ForDown(c: String, from: Expr, to: Expr, e: Expr) extends Expr

/**
  * The expression expr1 , … ,  exprn evaluates to
  * the n-tuple of the values of expressions expr1 to exprn.
  */
case class Tuple(l: Expr*) extends Expr

/**
  * The expression constr expr evaluates to the unary variant value whose
  * constructor is constr, and whose argument is the value of expr. 
  * Similarly, the expression constr (  expr1 , … ,  exprn ) evaluates to the n-ary variant value
  * whose constructor is constr and whose arguments are the values of expr1, …,  exprn.
  */
case class Constr(v: Name, e: Expr*) extends Expr

/**
  * The expression [ expr1 ; … ;  exprn ] is equivalent to expr1 :: … ::  exprn :: [], 
  * and therefore evaluates to the list whose elements are the values of expr1 to exprn.
  */
case class OList(val l: Expr*) extends Expr

/**
  * The expression `tag-name  expr evaluates to the polymorphic variant value
  * whose tag is tag-name, and whose argument is the value of expr.
  */
case class TaggedExpr(s: String, e: Expr) extends Expr //TODO

/**
  * The expression { field1 =  expr1 ; … ;  fieldn =  exprn } evaluates to the record value
  * { field1 = v1; …; fieldn = vn } where vi is the value of expri for i = 1,… , n. 
  */
case class Record(m: Map[Name,Expr]) extends Expr 

/**
  * The expression { expr with  field1 =  expr1 ; … ;  fieldn =  exprn } builds a fresh record
  * with fields field1 …  fieldn equal to expr1 …  exprn, 
  * and all other fields having the same value as in the record expr. 
  * In other terms, it returns a shallow copy of the record expr, 
  * except for the fields field1 …  fieldn, which are initialized to expr1 …  exprn.
  */
case class RecordCopy(r: Expr, update: Map[Name, Expr]) extends Expr

/**
  * The expression expr1 . field evaluates expr1 to a record value, 
  * and returns the value associated to field in this record value.
  */
case class RecordAccess(e: Expr, s: Name) extends Expr

/**
  * The expression expr1 . field <- expr2 evaluates expr1 to a record value, 
  * which is then modified in-place by replacing the value associated
  * to field in this record by the value of expr2. This operation is permitted only
  * if field has been declared mutable in the definition of the record type.
  * The whole expression expr1 . field <- expr2 evaluates to the unit value ().
  */
case class RecordUpdate(r: Expr, s: Name, e: Expr) extends Expr

/**
  * The expression [| expr1 ; … ;  exprn |] evaluates to a n-element array, 
  * whose elements are initialized with the values of expr1 to exprn respectively.
  */
case class Array(l: Expr*) extends Expr

/**
  * The expression expr1 .( expr2 ) returns the value of element
  * number expr2 in the array denoted by expr1. 
  * The first element has number 0; the last element has number n−1, 
  * where n is the size of the array. 
  * The exception Invalid_argument is raised if the access is out of bounds.
  */
case class ArrayAccess(array: Expr, i: Expr) extends Expr

/**
  * The expression expr1 .( expr2 ) <- expr3 modifies in-place the array denoted by expr1, 
  * replacing element number expr2 by the value of expr3. 
  * The exception Invalid_argument is raised if the access is out of bounds.
  * The value of the whole expression is ().
  */
case class ArrayUpdate(array: Expr, i: Expr, newValue: Expr) extends Expr

/**
  * The expression expr1 .[  expr2 ] returns the value of character number
  * expr2 in the string denoted by expr1. The first character has number 0; 
  * the last character has number n−1, where n is the length of the string. 
  * The exception Invalid_argument is raised if the access is out of bounds.
  */
case class CharOf(s: Expr, i: Expr) extends Expr  

/**
  * The expression expr1 .[  expr2 ] <-  expr3 modifies in-place the string denoted by expr1, 
  * replacing character number expr2 by the value of expr3. 
  * The exception Invalid_argument is raised if the access is out of bounds. 
  * The value of the whole expression is ().
  */
case class UpdateString(s: Expr , i: Expr, c: Expr) extends Expr

/**
  * prefix-symbol  expr is interpreted as the application ( prefix-symbol ) expr.
  */
case class UnaryOp(op: String, a: Expr) extends Expr

/**
  * The expression expr1 infix-symbol expr2 
  * is interpreted as the application ( infix-symbol ) expr1 expr2.
  */
case class InfixOp(a: Expr, op: String, b: Expr) extends Expr

/**
  * Expressions whose type contains object or polymorphic variant types can be explicitly coerced (weakened) to a supertype. 
  * The expression (expr :>  typexpr) coerces the expression expr to type typexpr. 
  * The expression (expr :  typexpr1 :>  typexpr2) coerces the expression expr from type typexpr1 to type typexpr2.
  */
case class Coercion(e: Expr, t1: Option[Type], t2: Type) extends Expr //TODO  (expr :  typexpr1 :>  typexpr2)


/**
  * begin expr end have the same value as expr
  */
case class BeginEnd(e: Expr) extends Expr

/**
  * Parenthesized expressions can contain a type constraint, 
  * as in ( expr :  typexpr ). This constraint forces the type of expr to be compatible with typexpr.
  */
case class Ascription(e: Expr, t: Type) extends Expr

/**
  * The expression
  * {{{
  * try expr 
  * with pattern1 -> expr1 
  * |	… 
  * |   patternn -> exprn
  * }}}
  * evaluates the expression expr and returns its value 
  * if the evaluation of expr does not raise any exception. 
  * If the evaluation of expr raises an exception, 
  * the exception value is matched against the patterns pattern1 to patternn.  
  */
case class Try(e: Expr, l: PatternMatching*) extends Expr

/**
  * When class-path evaluates to a class body, new class-path evaluates to a new object
  * containing the instance variables and methods of this class.
  * When class-path evaluates to a class function, new class-path evaluates to 
  * a function expecting the same number of arguments and returning a new object of this class.
  */
case class New(n: Name) extends Expr

/**
  * The expression expr #  method-name invokes the method method-name of the object denoted by expr.
  */
case class MethodCall(e: Expr, name: String) extends Expr 

/**
  * The instance variables of a class are visible only in the body 
  * of the methods defined in the same class or a class that inherits 
  * from the class defining the instance variables. 
  * The expression inst-var-name evaluates to the value of the given instance variable. 
  */
case class InstVar(name: String) extends Expr 

/**
  * The expression inst-var-name <-  expr assigns the value of expr to the instance variable inst-var-name, 
  * which must be mutable. The whole expression inst-var-name <-  expr evaluates to ().
  */
case class AssignInstVar(name: String, e : Expr) extends Expr

/**
  * Inside a method, the expression {< inst-var-name =  expr  { ; inst-var-name =  expr } >} returns a copy of self 
  * with the given instance variables replaced by the values of the associated expressions; 
  * other instance variables have the same value in the returned object as in self.
 */
case class SelfCopy(update: Map[String, Expr]) extends Expr 

/**
  * Creating directly an object through the object class-body end construct is operationally equivalent
  * to defining locally a class class-name = object class-body end 
  * and immediately creating a single object from it by new class-name.
  */
case class Object(body: ClassBody) extends Expr  //TODO test

case class ClassBody(cs: List[ClassField], p: Option[Pattern] = None, t : Option[Type] = None) 
abstract class ClassField
case class Inherit(ce: ClassExpr, as: Option[String] = None) extends ClassField //inherit class-expr  [as lowercase-ident]  
case class Val(name : String, e : Expr, mutable : Boolean = false, t: Option[Type] = None) extends ClassField //val [mutable] inst-var-name  [: typexpr] =  expr  
case class VirtualVal(name : String, t : Type, mutable : Boolean = false) extends ClassField // val [mutable] virtual inst-var-name :  typexpr  
case class Initializer(e: Expr) extends ClassField // initializer expr
case class Constraint (t1: Type, t2: Type) extends ClassField // constraint typexpr =  typexpr  
case class Method(name: String, args: List[Parameter], e: Expr, t: Option[Type], privatee : Boolean = false) extends ClassField //method [private] method-name  {parameter}  [: typexpr] =  expr  
case class PolyMethod(name : String, pt : PolyTypeExpr, e : Expr, privatee : Boolean = false)  extends ClassField //    method [private] method-name :  poly-typexpr =  expr  
case class VirtualMethod(name : String, pt: PolyTypeExpr, privatee : Boolean = false)  extends ClassField //  method [private] virtual method-name :  poly-typexpr   

sealed abstract class ClassExpr
