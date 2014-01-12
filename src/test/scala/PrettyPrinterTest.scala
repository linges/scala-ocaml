
package scalaocaml.text
import org.scalatest.FunSuite
import scala.io.Source
import scalaocaml._

class PrettyPrinterTest extends FunSuite with TestExamples
{

  def compare(result: Expr, expect: String) = {
    val a = cleanString(SyntaxPrettyPrinter.pretty(result))
    val b = cleanString(expect)
    println(a + " = " + b)
    assert(a == b)
  }

  def cleanString(s:String) = 
    s.replaceAll("[\n\r]","").replaceAll("  ", "").trim
}
