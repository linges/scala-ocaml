
package scalaocaml.text
import org.scalatest.FunSuite
import scala.io.Source
import scalaocaml._

class PrettyPrinterTest extends FunSuite with TestExamples
{

  def compare(result: Any, expect: String) = {
    val a = cleanString(SyntaxPrettyPrinter.pretty(result))
    val b = cleanString(expect)
    if(a != b){
        println(a + " \n" + b)
        println(SyntaxPrettyPrinter.pretty(result))
    }
    assert(a == b)
  }

  def cleanString(s:String) = 
    s.replaceAll("[\n\r]"," ").replaceAll("\\s+", " ").trim
}
