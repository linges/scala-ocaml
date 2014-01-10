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
case class LetFun(id: String,args: List[String], v: Expr) extends Expr

case class Pattern()

case class Conditional(condition: Expr, thenE: Expr, elseE: Expr) extends Expr



  /**
   * Pretty printer for SL definitions and expressions.
   */
object SyntaxPrettyPrinter extends PrettyPrinter {
  
  def pretty(t: Any): String = t match {
    case e: Expr => super.pretty(showExpr(e))
    case e => pretty_any(e)
  }

  def showExpr(e: Expr) : Doc = e match {
    case OInt(v) => value(v)
    case OFloat(v) => value(v)
    case True() => "true"
    case False() => "false"
    case OChar(c) => value(c)
    case OString(s) => value(s)
    case Var(v) => v
    case OList(l) => brackets( lsep(l.map(showExpr), semi) )
    case Conditional(c, t, e) => "if" <+> showExpr(c) <@> "then" <+>
      nest(line <> showExpr(t)) <@> "else" <> nest(line <> showExpr(e))
  }
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
