package scalaocaml

import org.kiama.output.PrettyPrinter

sealed abstract class Expr

//Data types

case class OInt(n: Int) extends Expr
case class OFloat(n: Double) extends Expr

sealed abstract class Boolean extends Expr
case class True() extends Boolean
case class False() extends Boolean

case class OChar(c: Char) extends Expr
case class OString(s: String) extends Expr 

case class Var(v: String) extends Expr

case class OList(l: List[Expr]) extends Expr

case class Let(p: Pattern, v: Expr) extends Expr
case class LetFun(id: String, args: List[String], v: Expr) extends Expr

abstract class Pattern
case class PVar(v: String) extends Pattern

case class Conditional(condition: Expr, thenE: Expr, elseE: Expr) extends Expr

case class InfixOp(a: Expr, op: String, b: Expr) extends Expr
case class App(f: Expr, x: Expr) extends Expr
case class Tuple(l: List[Expr]) extends Expr


  /**
   * Pretty printer for SL definitions and expressions.
   */
object SyntaxPrettyPrinter extends PrettyPrinter {
  
  def pretty(t: Any): String = t match {
    case e: Expr => super.pretty(showExpr(e))
    case e => pretty_any(e)
  }

  implicit def showExpr(e: Expr) : Doc = e match {
    case OInt(v) => value(v)
    case OFloat(v) => value(v)
    case True() => "true"
    case False() => "false"
    case InfixOp(a,o,b) => a <+> text(o) <+> b 
    case App(f,x) => parens(f <+> x)
    case OChar(c) => value(c)
    case OString(s) => '"' <> string(s) <> '"'
    case Var(v) => v
    case OList(l) => brackets( lsep(l.map(showExpr), semi) )
    case Conditional(c, t, e) => "if" <+> showExpr(c) <+> "then" <+>
      nest(line <> showExpr(t)) <+> "else" <+> nest(line <> showExpr(e))
    case Let(p, v) => group("let" <+> p <+> "=" <> nest(line <> v))
    case LetFun(id, l, v) =>  group("let" <+> l.foldLeft(text(id)){ _ <+> _ } 
                              <+> "=" <> nest(line <> v))
    case Tuple(l) => list(l, "", showExpr)
  }

  implicit def showPattern(e: Pattern) : Doc = e match {
    case PVar(v) => value(v)
  }

  def catList(l: List[Doc], sep: Doc): Doc = (group(nest(lsep(l, sep))))
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
