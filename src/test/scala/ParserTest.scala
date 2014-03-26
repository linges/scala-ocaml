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

  def compareIdentifier(term: Identifier, input: String) = {
    OCamlParser.parseIdentifier(input) match {
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

  def compareClassType(term: ClassType, input: String) = {
    OCamlParser.parseClassType(input) match {
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

  def compareClassExpr(term: ClassExpr, input: String) = {
    OCamlParser.parseClassExpr(input) match {
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


  def compareModuleExpr(term: ModuleExpr, input: String) = {
    OCamlParser.parseModuleExpr(input) match {
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

  def compareModuleType(term: ModuleType, input: String) = {
    OCamlParser.parseModuleType(input) match {
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

  def compareUnitImplementation(term: UnitImplementation, input: String) = {
    OCamlParser.parseUnitImplementation(input) match {
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

  def compareUnitInterface(term: UnitInterface, input: String) = {
    OCamlParser.parseUnitInterface(input) match {
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
    compareExpr(result, expect)
  }

  test("priorities 2") {
    val result = InfixOp(InfixOp(OInt(2), "*", OInt(3)), "+", OInt(4))
    val expect = "2 * 3 + 4"
    compareExpr(result, expect)
  }

  test("left associative") {
    val result = InfixOp(InfixOp(OInt(2), "*", OInt(3)), "*", OInt(4))
    val expect = "2 * 3 * 4"
    compareExpr(result, expect)
  }

  test("right associative") {
    val result = InfixOp(OInt(2), "^",InfixOp(OInt(3), "^", OInt(4)))
    val expect = "2 ^ 3 ^ 4"
    compareExpr(result, expect)
  }

  test("performance bug") {
    val result = OInt(2)
    val c = 500 
    val expect =  "("*c + "2" + ")"*c 
    compareExpr(result, expect)
  }
  /**
    * Comments
    */

  test("comment") {
    val result = OInt(2)
    val expect = "(***** comment  ****) 2"
    compareExpr(result, expect)
  }

  test("multi line comments") {
    val result = OInt(2)
    val expect = """
    (*
     *
     * comment  *) 2"""
    compareExpr(result, expect)
  }
}
