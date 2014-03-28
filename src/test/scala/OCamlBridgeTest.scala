package scalaocaml.test
import org.scalatest.FunSuite
import scala.io.Source
import scalaocaml._

class OCamlBridgeTest extends OCamlInterpreterBridge with FunSuite 
{

  test("eval expressions")
  {
    val input = TopLevelInput(OInt(3), OString("as"))
    val (output, errors) = this.runInterpreter(input)
    assert(errors == "")
  }

  test("eval directive")
  {
    val input = TopLevelInput(TopLevelDirective("labels", Some(True)))
    val (output, errors) = this.runInterpreter(input)
    assert(errors == "")
  }

  test("hello world script")
  {
    val input = """print_string "Hello world!\n";;"""
    val (output, errors) = this.runScript(input)
    assert(errors == "")
    assert(output == "Hello world!\n")
  }

  test("script with error")
  {
    val input = """print_string "Hello world!\n"""
    val (output, errors) = this.runScript(input)
    assert(errors.contains("Error"))
    assert(output == "")
  }
}
