
package scalaocaml.test
import org.scalatest.FunSuite
import scala.io.Source
import scalaocaml._

class PrettyPrinterTest extends FunSuite with TestExamples
{

  def compare(result: Any, expect: String) = {
    val a = cleanString(OCamlPrettyPrinter.pretty(result))
    val b = cleanString(expect)
    if(a != b){
        println(a + " \n" + b)
        println(OCamlPrettyPrinter.pretty(result))
    }
    assert(a == b)
  }


  def compareType(result: Type, expect: String) = compare(result, expect)
  def compareExpr(result: Expr, expect: String) = compare(result, expect)
  def compareClassExpr(result: ClassExpr, expect: String) = compare(result, expect)
  def compareClassType(result: ClassType, expect: String) = compare(result, expect)
  def compareDef(result: Definition, expect: String) = compare(result, expect)
  def compareIdentifier(result: Identifier, expect: String) = compare(result, expect)

  def cleanString(s:String) = 
    s.replaceAll("[\n\r]"," ").replaceAll("\\s+", " ").trim
}
