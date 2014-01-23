package scalaocaml

import org.kiama.output.PrettyPrinter


object OCamlPrettyPrinter extends OCamlPrettyPrinter 

/**
 * Pretty printer for OCaml definitions and expressions.
 */
trait OCamlPrettyPrinter extends PrettyPrinter with ExprPrettyPrinter 
    with TypePrettyPrinter with PatternPrettyPrinter
    with ClassPrettyPrinter with ModulePrettyPrinter
    with ConstantPrettyPrinter with IdentifierPrettyPrinter{
  
  def pretty(t: Any): String = t match {
    case e: Type => super.pretty(showType(e))
    case e: Expr => super.pretty(showExpr(e))
    case e: Definition => super.pretty(showDefinition(e))
    case e: Pattern => super.pretty(showPattern(e))
    case e: ClassType => super.pretty(showClassType(e))
    case e: ClassExpr => super.pretty(showClassExpr(e))
    case e: ModuleType => super.pretty(showModuleType(e))
    case e: ModuleExpr => super.pretty(showModuleExpr(e))
    case e: Identifier => super.pretty(showIdentifier(e))
    case e => pretty_any(e)
  }


  def catList(l: Seq[Doc], sep: Doc): Doc = (group(lsep(l.toList, sep)))
}
