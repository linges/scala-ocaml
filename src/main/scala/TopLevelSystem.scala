package scalaocaml
import scala.sys.process._
import java.io._
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex
import scala.language.postfixOps
import scala.language.implicitConversions

/** This describes the toplevel system for OCaml, that permits interactive use of the OCaml system through a read-eval-print loop. 
  * In this mode, the system repeatedly reads OCaml phrases from the input, 
  * then typechecks, compile and evaluate them, then prints the inferred type and result value, if any.
 */

case class TopLevelInput(tis: TopLevelPhrase*) 

trait TopLevelPhrase
trait DirectiveArgument

case class TopLevelDirective(directive: String, arg: Option[DirectiveArgument])  extends TopLevelPhrase


trait TopLevelSystemPrettyPrinter {
  self: OCamlPrettyPrinter =>

  implicit def showTopLevelInput(t: TopLevelInput) : Doc = lsep(t.tis.map(showTopLevelPhrase),";;" <> line) <> ";;" <> line

  def showTopLevelPhrase(e: TopLevelPhrase) : Doc = e match {
    case d : Definition => showDefinition(d)
    case e : Expr => showExpr(e)
    case TopLevelDirective(d, arg) => "#" <+> d <+> arg.map(showDirectiveArgument(_)).getOrElse(empty)
  }

  def showDirectiveArgument(dir: DirectiveArgument) : Doc  = dir match {
    case s: Constant => showConstant(s)
    case n: Name => showIdentifier(n)
  }
}
