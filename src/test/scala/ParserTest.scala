
package scalaocaml.test
import org.scalatest.FunSuite
import scala.io.Source
import scalaocaml._

class ParserTest extends FunSuite with TestExamples
{

  def compare(term: Any, input: String) = {
    
    OCamlParser.parseAny(input) match {
      case Right(t) =>
        if(t != term)
          {
            println(t)
            println(term)
          }
        assert(t == term)
      case Left(msg) => throw new Exception(msg)
    }
  }

  def compareType(term: Type, input: String) = {
    OCamlParser.parseType(input) match {
      case Right(t) =>
        if(t != term)
          {
            println(t)
            println(term)
          }
        assert(t == term)
      case Left(msg) => throw new Exception(msg)
    }
  }

  def compareDef(term: Definition, input: String) = {
    OCamlParser.parseDef(input) match {
      case Right(t) =>
        if(t != term)
          {
            println(t)
            println(term)
          }
        assert(t == term)
      case Left(msg) => throw new Exception(msg)
    }
  }

  def compareExpr(term: Expr, input: String) = {
    OCamlParser.parseExpr(input) match {
      case Right(t) =>
        if(t != term)
          {
            println(t)
            println(term)
          }
        assert(t == term)
      case Left(msg) => throw new Exception(msg)
    }
  }

  def cleanString(s:String) = 
    s.replaceAll("[\n\r]"," ").replaceAll("\\s+", " ").trim

  test("priorities") {
    val result = InfixOp(OInt(2), "+",InfixOp(OInt(3), "*", OInt(4)))
    val expect = "2 + 3 * 4"
    compare(result, expect)
  }

  test("priorities 2") {
    val result = InfixOp(InfixOp(OInt(2), "*", OInt(3)), "+", OInt(4))
    val expect = "2 * 3 + 4"
    compare(result, expect)
  }

  test("left associative") {
    val result = InfixOp(InfixOp(OInt(2), "*", OInt(3)), "*", OInt(4))
    val expect = "2 * 3 * 4"
    compare(result, expect)
  }

  test("right associative") {
    val result = InfixOp(OInt(2), "^",InfixOp(OInt(3), "^", OInt(4)))
    val expect = "2 ^ 3 ^ 4"
    compare(result, expect)
  }

  test("performance bug") {
    val result = OInt(2)
    val c = 500 
    val expect =  "("*c + "2" + ")"*c 
    compareExpr(result, expect)
  }
}
