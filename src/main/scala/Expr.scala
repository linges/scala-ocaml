package scalaocaml

import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.language.implicitConversions


/**
  * A value definition let [rec] let-binding  { and let-binding } bind 
  * value names in the same way as a let … in … expression.
  */
case class Let(lb: LetBinding*) extends Definition
case class LetRec(lb: LetBinding*) extends Definition  


trait Expr extends Argument with ModuleItem

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
case class App(f: Expr, x: Argument, xs : Argument*) extends Expr


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
case class Fun(pars: List[Parameter], e: Expr, guard : Option[Expr] = None) extends Expr 
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
case class TaggedExpr(s: String, e: Expr) extends Expr 

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
case class OArray(l: Expr*) extends Expr

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
case class Coercion(e: Expr, t1: Option[Type], t2: Type) extends Expr


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
case class Object(body: ClassBody) extends Expr 

trait Argument 
case class LabeledArg(name: String, e: Option[Expr] = None) extends Argument
case class OptionalLabeledArg(name: String, e: Option[Expr] = None) extends Argument

sealed abstract class LetBinding
case class Binding(p: Pattern, e: Expr) extends LetBinding
case class FunBinding(name : String, args: List[Parameter], e: Expr, t: Option[Type] = None, t2: Option[Type] = None) extends LetBinding 

trait ExprPrettyPrinter {
  self: OCamlPrettyPrinter =>

  implicit def showExpr(e: Expr) : Doc = e match {
    case c: Constant          => showConstant(c)
    case InfixOp(a,o,b)       => parens(a <+> text(o) <+> b)
    case UnaryOp(o,a)         => parens(text(o) <+> a)
    case App(f,x)             => parens(f <+> x)
    case App(f,x, xs@ _*)     => parens(f <+> x <+> catList(xs.map(showArg), space))
    case Fun(args,e,None)     => parens("fun" <+> catList(args.map(showParameter), "") <+> "->" <+> e)
    case Fun(args,e,Some(g))  => parens("fun" <+> catList(args.map(showParameter), "") <+>
        "when"<+> g <+> "->" <+> e)
    case Function(ps@ _*)     => "function" <+> lsep(ps.map(showPatternMatching), line)
    case Var(v)               => v
    case OList(l@ _*)         => brackets( catList(l.map(showExpr), semi) )
    case IfThenElse(c, t, e)  => "if" <+> showExpr(c) <+> "then" <>
      nest(line <> showExpr(t)) <@> "else" <> nest(line <> showExpr(e))
    case IfThen(c, t)         => "if" <+> showExpr(c) <+> "then" <>
      nest(line <> showExpr(t))
    case LetIn(v, e)          => group("let" <+> lsep(v.map(showLetBinding), line <>"and") <@> "in" <> nest(line <> e))
    case LetRecIn(v, e)       => group("let rec" <+> lsep(v.map(showLetBinding), line <>"and") <@> "in" <> nest(line <> e))
    case Tuple(l@ _*)         => list(l.toList, "", showExpr)
    case Constr(n)            => n
    case Constr(n, l@ _*)     => n <> list(l.toList, "", showExpr)
    case Match(e, bs@ _*)     => "match" <+> e <+> "with" <>
      nest(line <> bs.map(showPatternMatching).reduce(_ <@> _))
    case Try(e, bs@ _*)       => "try" <+> e <+> "with" <>
      nest(line <> bs.map(showPatternMatching).reduce(_ <@> _))
    case Record(m)            => enclose("{",  catList(m.map{
      case (s,t)              => s <+> "=" <+> t }.toList, semi)
        ,"}")
    case RecordCopy(e,m)      => enclose("{", e <+> "with" <+>
        catList(m.map{
          case (s,t)          => s <+> "=" <+> t }.toList, semi)
        ,"}")
    case RecordAccess(e,s)    => e <> dot <> s 
    case RecordUpdate(r,s,e)  => r <> dot <> s <+> "<-" <+> e
    case ForTo(i,from,to,e)   => "for" <+> i <+> "=" <+> from <+> "to" <+> to <+> "do" <>
      nest(line <> e) <@> "done"
    case ForDown(i,from,to,e) => "for" <+> i <+> "=" <+> from <+> "downto" <+> to <+> "do" <>
      nest(line <> e) <@> "done"
    case While(e,b)           => "while" <+> e <+> "do" <> nest(line <> b) <@> "done"
    case Sequence(l@ _*)      => catList(l.map(showExpr), semi)
    case Ascription(e,t)      => parens(e<+>":"<+>t)
    case OArray(l@ _*)         => enclose("[|", catList(l.map(showExpr), semi), "|]")
    case ArrayAccess(a,e)     => a <> dot <> parens(e) 
    case ArrayUpdate(a,i,e)   => a <> dot <> parens(i) <+> "<-" <+> e
    case New(n)               => "new" <+> n
    case CharOf(s,i)          => s <> dot <> brackets(i)
    case UpdateString(s,i,c)  => s <> dot <> brackets(i) <+> "<-" <+> c
    case BeginEnd(e)          => "begin" <@> e <@> "end"
    case MethodCall(e, s)     => e <+> "#" <+> s
    case InstVar(s)           => value(s)
    case AssignInstVar(s, e)  => value(s) <+> "<-" <+> e
    case Object(b)            => "object" <+> b <@> "end"
    case TaggedExpr(name, e)  => parens("`" <> name <+> e)
    case Coercion(n, None, t2) => parens(n <+> ":>" <+> t2)
    case Coercion(n, Some(t), t2) => parens(n <+> ":" <+> t <+> ":>" <+> t2)
  }

  implicit def showArg(a: Argument) : Doc = a match {
    case e : Expr => showExpr(e)
    case LabeledArg(n, None) => "~" <> n
    case LabeledArg(n, Some(e)) => "~" <> n <> ":" <> e
    case OptionalLabeledArg(n, None) => "?" <> n
    case OptionalLabeledArg(n, Some(e)) => "?" <> n <> ":" <> e
  }

  implicit def showLetBinding(lb: LetBinding) : Doc = lb match {
    case Binding(p,e)               => p <+> "=" <+> e
    case FunBinding(n, l, e, t, t2) => n <+> catList(l.map(showParameter), space) <>
        t.map(" :"<+> _).getOrElse("") <> t2.map(" :>"<+> _).getOrElse("") <+> "=" <+> e
  }
}

trait ExprParser extends RegexParsers with Parsers {
  self: OCamlParser =>



  lazy val simpleexpr: Parser[Expr] =  list | constant | record | vari | array | forto |
                                       fordown | whiledone | beginend | onew | tagged |
                                       omatch | ascription | coercion | recordcopy 

  lazy val ascription: Parser[Expr] = ("(" ~> expr <~ ":") ~ typeexpr <~ ")" ^^ { case e~t => Ascription(e,t) }

  lazy val coercion: Parser[Expr] = ("(" ~> expr ~ ((":" ~> typeexpr).?) ~ (":>" ~> typeexpr) <~ ")") ^^ { case e~o~t => Coercion(e,o,t) }

  lazy val beginend: Parser[Expr] = "begin" ~>  expr <~ "end" ^^ { x => BeginEnd(x) }

  lazy val onew = "new" ~> name ^^ { x => New(x) }

  def methodcall(e:Expr): Parser[Expr] = "#" ~> lowercaseident ^^ { case i => MethodCall(e,i) } | success(e)

  lazy val forto: Parser[Expr] = ("for" ~> ident <~ "=") ~ (expr <~ "to") ~ 
                            (expr <~ "do") ~ expr <~ "done" ^^
                            { case v~f~t~d => ForTo(v,f,t,d) }

  lazy val fordown: Parser[Expr] = ("for" ~> ident <~ "=") ~ (expr <~ "downto") ~ 
                              (expr <~ "do") ~ expr <~ "done" ^^
                              { case v~f~t~d => ForDown(v,f,t,d) }

  lazy val whiledone: Parser[Expr] = ("while" ~> expr <~ "do") ~ expr <~ "done" ^^ 
                                { case e~d => While(e,d) }

  lazy val vari = valuepath ^^ {Var(_)}

  lazy val record = "{" ~> rep1sep(recordField, ";") <~ "}" ^^
                              { case l : List[(Name,Expr)] => Record(l.toMap) }

  lazy val recordcopy = "{" ~> (expr <~ "with") ~ rep1sep(recordField, ";") <~ "}" ^^
                              { case e~l  => RecordCopy(e, l.toMap) }

  lazy val recordField: Parser[(Name, Expr)] = (name <~ "=") ~ lvl2 ^^ { case n~e => (n,e) }

  lazy val array = "[|" ~> rep1sep(lvl2, ";") <~ (";" ?) <~ "|]" ^^
                            { case l => OArray(l:_*) }
  
  lazy val list = "[" ~> rep1sep(lvl2, ";") <~ (";" ?) <~ "]" ^^
                            { case l => OList(l:_*) }

  lazy val ifthenelse: Parser[Expr] = ("if" ~> expr <~ "then") ~ (expr <~ "else") ~ lvl2 ^^
                                 { case c ~ t ~ e => IfThenElse(c,t,e) }

  lazy val ifthen: Parser[Expr] = ("if" ~> expr <~ "then")  ~ lvl2 ^^
                                 { case c ~ t => IfThen(c,t) }

  lazy val tuple = (lvl5 <~ ",") ~ repsep(lvl5, ",")  ^^
                             { case e~l => Tuple(e::l:_*) } 

  lazy val tagged = "`" ~> ident ~ expr ^^ { case i~e => TaggedExpr(i,e) }

  def selection(e:Expr) : Parser[Expr] = (stringdot1(e) into arraydot1) into recorddot1

  def recorddot1(e:Expr): Parser[Expr] = ((("." ~> name ) into 
                    { i:Name => recorddot2(e,i) }) into selection) | success(e)
  def recorddot2(a: Expr, i: Name): Parser[Expr] = "<-" ~> lvl9 ^^ {case e => RecordUpdate(a,i,e) } |  
                                               success(RecordAccess(a,i))

  def arraydot1(e: Expr): Parser[Expr] = ((("."~"(" ~> expr <~ ")") into 
                     { i:Expr => arraydot2(e,i) }) into selection) | success(e)
  def arraydot2(a: Expr, i: Expr): Parser[Expr] = "<-" ~> lvl9 ^^ {case e => ArrayUpdate(a,i,e) } |
                                               success(ArrayAccess(a,i))

  def stringdot1(e: Expr): Parser[Expr] = ((("."~"[" ~> expr <~ "]") into 
                     { i:Expr => stringdot2(e,i) }) into selection) | success(e)
  def stringdot2(a: Expr, i: Expr): Parser[Expr] = "<-" ~> lvl9 ^^ {case e => UpdateString(a,i,e) } |
                                               success(CharOf(a,i))

  lazy val instvarassign: Parser[Expr] = (lowercaseident <~ "<-") ~ expr ^^ { case n~e => AssignInstVar(n,e) }

  lazy val parentheses: Parser[Expr] = "(" ~> expr <~ ")"
  lazy val sequence  = (lvl2 <~ ";")  ~ repsep(lvl2, ";") ^^ { case e~l => Sequence(e::l:_*) }

  lazy val letin: Parser[LetIn] = ("let" ~> rep1sep(letbinding, "and") <~ "in") ~ expr ^^
                             { case l~e => LetIn(l,e) }
  lazy val letrecin: Parser[LetRecIn] = ("let" ~> "rec" ~> rep1sep(letbinding, "and") <~ "in") ~ expr ^^
                             { case l~e => LetRecIn(l,e) }
  lazy val let = ("let" ~> rep1sep(letbinding, "and") ) ^^
                             { case l => Let(l:_*) }
  lazy val letrec = ("let" ~> "rec" ~> rep1sep(letbinding, "and"))  ^^
                             { case l => LetRec(l:_*) }

  lazy val prefix = (prefixsymbol) ~ lvl10 ^^ { case s~e => UnaryOp(s,e) } 

  lazy val omatch = ("match" ~> expr) ~ ("with" ~> patternmatching) ^^ { case e~ps => Match(e,ps:_*) }

  lazy val neg = ("-"|"-.") ~ lvl7 ^^ { case o~e => UnaryOp(o, e) } 

  lazy val otry : Parser[Expr] = ("try" ~> expr <~ "with") ~ patternmatching ^^ { case e~ps => Try(e, ps:_*) }

  def fun : Parser[Fun] = "fun" ~> rep(parameter) ~ (("when" ~> expr).?) ~ ("->" ~> expr) ^^ {case ps~g~e => Fun(ps,e,g) }
 
  //We use this do represent priorities.
  //Higher level means higher priority.
  //It has the nice side effect that the compiler forbids unwanted recursion,
  //if we don't specify a type for the parser.
  lazy val lvl0 = letin | letrecin | otry | lvl1 | fun
  lazy val lvl1 = sequence | lvl2
  lazy val lvl2 = ifthenelse | ifthen | lvl3
  lazy val lvl3 = instvarassign | lvl4 // <- :=
  lazy val lvl4 = tuple | lvl5 
  lazy val lvl5 = binop | lvl6
  lazy val lvl6 = neg | lvl7 //(prefix)
  lazy val lvl7 = app | constr | lvl8
  lazy val lvl8 = (lvl9 into selection) into methodcall
  lazy val lvl9 = prefix | lvl10
  lazy val lvl10 = (simpleexpr | parentheses) into methodcall

  lazy val expr = lvl0 



  lazy val app = lvl8 ~ rep1(arg) ^^ { case f ~ l => App(f, l.head, l.tail :_*)}
  lazy val constr = constrpath ~ rep(lvl8) ^^ { case f ~ l => Constr(f, l:_*)}

  lazy val arg : Parser[Argument] = labeledarg | optionallabeledarg | lvl8
  lazy val labeledarg = ("~" ~>  labelname ~ (":" ~> lvl8).?) ^^ { case l~e => LabeledArg(l,e) }
  lazy val optionallabeledarg = ("?" ~>  labelname ~ (":" ~> lvl8).?) ^^ 
                                { case l~e => OptionalLabeledArg(l,e) }



// case class FunBinding(name : String, args: List[Parameter], e: Expr, t: Option[Type] = None, t2: Option[Type] = None) extends LetBinding 
  lazy val letbinding: Parser[LetBinding] = binding | funbinding
  lazy val binding: Parser[Binding] = (pattern <~ "=") ~ expr ^^ { case p~e => Binding(p,e) } 
  //TODO typexpr
  lazy val funbinding: Parser[FunBinding] = valuename ~ (parameter *) ~  ( "=" ~> expr) ^^ { case n~ps~e => FunBinding(n, ps, e) } 
  /*
    case Function(ps@ _*)     => "function" <+> lsep(ps.map(showPatternMatching), line)
    case Object(b)            => "object" <+> b <@> "end"
    case Constr(n)            => n
    case Constr(n, l@ _*)     => n <> list(l.toList, "", showExpr)
    case RecordCopy(e,m)      => enclose("{", e <+> "with" <+>
        catList(m.map{
          case (s,t)          => s <+> "=" <+> t }.toList, semi)
        ,"}")
    case Ascription(e,t)      => parens(e<+>":"<+>t)
    case MethodCall(e, s)     => e <+> "#" <+> s
    case Coercion(n, None, t2) => parens(n <+> ":>" <+> t2)
    case Coercion(n, Some(t), t2) => parens(n <+> ":" <+> t <+> ":>" <+> t2)

   */
  //Shunting-yard algorithm TODO associative 
  lazy val binop = lvl6 ~ rep1(infixop ~ lvl6) ^^ {
    case x ~ xs =>
      var input = new Queue ++= (x :: (xs.flatMap({ case a ~ b => List(a, b) })))
      val out: Stack[Expr] = new Stack
      val ops: Stack[String] = new Stack
      var isOp = false
      while (!input.isEmpty) {
        val o1 = input.dequeue
        if (isOp) {
          while (!ops.isEmpty && prec(o1) <= prec(ops.head)) {
            clearStack(out, ops)
          }
          ops.push(o1.asInstanceOf[String])
        } else {
          out.push(o1.asInstanceOf[Expr])
        }
        isOp = !isOp
      }
      while (!ops.isEmpty) clearStack(out, ops)
      if (out.size != 1) failure("OutputStack should have only one value")
      out.pop
  }

  private def clearStack(out: Stack[Expr], ops: Stack[String]) =
    {
      val o2 = ops.pop
      val b = out.pop
      val a = out.pop

      out.push(InfixOp(a,o2,b))

    }

  //TODO precedences and associativity
  private def prec(op: Any): Int = op match {
    case _ => 0
  }
}
