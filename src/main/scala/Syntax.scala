package scalaocaml

import org.kiama.output.PrettyPrinter

abstract class TopLevel
case class TypeDecl(tyvars: List[String], id: String, t: Type) extends TopLevel
case class Let(p: Pattern, v: Expr) extends TopLevel
case class LetFun(id: String, args: List[Pattern], v: Expr) extends TopLevel
case class LetRec(id: String, args: List[Pattern], v: Expr) extends TopLevel

sealed abstract class Expr

case class OInt(n: Int) extends Expr
case class OFloat(n: Double) extends Expr

sealed abstract class Boolean extends Expr
case class True() extends Boolean
case class False() extends Boolean

case class OChar(c: Char) extends Expr
case class OString(s: String) extends Expr 

case class Var(v: String) extends Expr

case class OList(val l: Expr*) extends Expr

case class LetIn(p: Pattern, v: Expr, e: Expr) extends Expr

case class Fun(args: List[String], e: Expr) extends Expr

case class Conditional(condition: Expr, thenE: Expr, elseE: Expr) extends Expr

case class InfixOp(a: Expr, op: String, b: Expr) extends Expr
case class PrefixBinaryOp(op: String, a: Expr, b: Expr) extends Expr
case class UnaryOp(op: String, a: Expr) extends Expr
case class App(f: Expr, x: Expr) extends Expr
case class Tuple(l: Expr*) extends Expr
case class Match(e: Expr, l: Branch*) extends Expr
case class Branch(p: Pattern, e: Expr)

case class Record(m: Map[String,Expr]) extends Expr 

case class RecordAccess(e: Expr, s: String) extends Expr
case class RecordUpdate(r: Expr, s: String, e: Expr) extends Expr

case class Sequence(l: Expr*) extends Expr

case object Unit extends Expr

abstract class Type
case class TVar(v : String) extends Type
case class PolyVar(v : String) extends Type
case class TRecord(m: RecordField*) extends Type
abstract class RecordField
case class MutableRecordField(s: String,e: Type) extends RecordField
case class ImmutableRecordField(s: String,e: Type) extends RecordField

case class TVariant(v: Variant*) extends Type
abstract class Variant 
case class VariantField(name: String, t: Type) extends Variant
case class Constant(name: String) extends Variant

abstract class Pattern
case class PVar(v: String) extends Pattern
case class PTuple(l: Pattern*) extends Pattern 
case class PList(l: Pattern*) extends Pattern
case class PRecord(m: Map[String, Pattern]) extends Pattern
case class PVariant(constr: String, p: Pattern*) extends Pattern
case class OrPattern(a: Pattern, b: Pattern) extends Pattern
case class FixSizeList(l: List[Pattern]) extends Pattern
case object Underscore extends Pattern
case object PUnit extends Pattern


/**
 * Pretty printer for OCaml definitions and expressions.
 */
object SyntaxPrettyPrinter extends PrettyPrinter {
  
  def pretty(t: Any): String = t match {
    case e: Type => super.pretty(showType(e))
    case e: Expr => super.pretty(showExpr(e))
    case e: TopLevel => super.pretty(showTopLevel(e))
    case e => pretty_any(e)
  }

  implicit def showTopLevel(e: TopLevel) : Doc = e match {
    case Let(p, v) => group("let" <+> p <+> "=" <> nest(line <> v))
    case LetFun(id, l, v) =>  group("let" <+> l.foldLeft(text(id)){ _ <+> _ } 
                              <+> "=" <> nest(line <> v))
    case LetRec(id, l, v) =>  group("let rec" <+> l.foldLeft(text(id)){ _ <+> _ } 
                              <+> "=" <> nest(line <> v))
    case TypeDecl(Nil, id, t) =>  group("type" <+> id <+> "=" <> nest(line <> t))
    case TypeDecl(tvs, id, t) =>  group("type" <+> parens(catList(tvs.map("'"<>_), comma))
        <+> id <+> "=" <> nest(line <> t))
  }

  implicit def showType(e: Type) : Doc = e match {
    case TVar(v) => value(v)
    case PolyVar(v) => "'"<>value(v)
    case TRecord(m@ _*) => enclose("{",  catList(m.map(showRecordField), semi)
        ,"}")
    case TVariant(l@ _*) => "|" <+> catList(l.map(showVariant), "|" )
  }

  implicit def showVariant(v: Variant) : Doc = v match {
    case VariantField(n,t) => n <+> "of" <+> t
    case Constant(n)  => n
  }

  implicit def showExpr(e: Expr) : Doc = e match {
    case OInt(v) => value(v)
    case OFloat(v) => value(v)
    case True() => "true"
    case False() => "false"
    case InfixOp(a,o,b) => parens(a <+> text(o) <+> b)
    case PrefixBinaryOp(o,a,b) => parens(parens(text(o)) <+> a <+> b)
    case UnaryOp(o,a) => parens(text(o) <+> a)
    case App(f,x) => parens(f <+> x)
    case Fun(args,e) => parens("fun" <+> catList(args.map(value), space) <+> "->" <+> e)
    case OChar(c) => value(c)
    case OString(s) => '"' <> string(s) <> '"'
    case Var(v) => v
    case OList(l@ _*) => brackets( catList(l.map(showExpr), semi) )
    case Conditional(c, t, e) => "if" <+> showExpr(c) <+> "then" <>
      nest(line <> showExpr(t)) <@> "else" <> nest(line <> showExpr(e))
    case LetIn(p, v, e) => group("let" <+> p <+> "=" <> nest(line <> v) <@> "in" <> nest(line <> e))
    case Tuple(l@ _*) => list(l.toList, "", showExpr)
    case Match(e, bs@ _*) => "match" <+> e <+> "with" <>
      nest(line <> bs.map(showBranch).reduce(_ <@> _))
    case RecordAccess(e,s) => e <> dot <> s 
    case Record(m) => enclose("{",  catList(m.map{
      case (s,t) => s <+> "=" <+> t }.toList, semi)
        ,"}")
    case Sequence(l@ _*) => catList(l.map(showExpr), semi)
    case RecordUpdate(r,s,e) => r <> dot <> s <+> "<-" <+> e
    case Unit => "()"
  }

  implicit def showBranch(b: Branch) = b match {
    case Branch(p,e) => "|"<+> p <+> "->" <+> e
  }
  implicit def showRecordField(rf: RecordField) : Doc = rf match {
    case ImmutableRecordField(s, t) => s <+> ":" <+> t
    case MutableRecordField(s, t) => "mutable" <+> s <+> ":" <+> t
  }

  implicit def showPattern(e: Pattern) : Doc = e match {
    case PVar(v) => value(v)
    case PTuple(l@ _*) => list(l.toList, "", showPattern)
    case PList(l@ _*) => if (l.isEmpty) "[]" else list(l.toList, "", showPattern, space<>"::")
    case PRecord(m) => enclose("{",  catList(m.map{
      case (s,t) => s <+> "=" <+> t }.toList, semi)
        ,"}")
    case PVariant(n, p@ _*) => n <> parens(catList(p.map(showPattern),comma))
    case OrPattern(a,b) => a <+> "|" <+> b
    case FixSizeList(l) => brackets( catList(l.map(showPattern), semi) )
    case Underscore => "_"
    case PUnit => "()"
  }

  def catList(l: Seq[Doc], sep: Doc): Doc = (group(nest(lsep(l.toList, sep))))
  /*
    def showExpr(t: Expr): Doc = t match {
      case Conditional(c, t, e, a) => ifLex <+> showExpr(c) <@> thenLex <+> nest(line <> showExpr(t)) <@> elseLex <> nest(line <> showExpr(e))
      case Lambda(ps, e, a) => parens(lambdaLex <+> catList(ps.map(showPattern), "") <+> dotLex <> nest(line <> showExpr(e)))
      case Case(e, as, a) => caseLex <+> showExpr(e) <@> ssep(as.map(showAlt), linebreak)
      case Let(ds, e, a) => letLex <+> nest(line <> cat(ds.map(showLefDef))) <@> inLex <> nest(line <> showExpr(e))
      case App(f, e, a) => parens(showExpr(f) <+> showExpr(e))
      case ExVar(i, a) => i.toString
      case ExCon(c, a) => c.toString
      case ConstInt(v, a) => value(v)
      case ConstChar(c, a) => dquotes(value(c))
      case ConstString(s, a) => dquotes(value(s))
      case ConstReal(x, _) => x.toString
    }
   */
}
