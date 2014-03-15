package scalaocaml
import scala.sys.process._
import java.io._

object Test {
  def main(args: Array[String]) {
    val oib = new OCamlInterpreterBridge()
    val input = TopLevelInput(OInt(3), OString("as"))
    val (output, errors) = oib.runInterpreter(input)
    print(output)
    print(errors)
  }
}

class OCamlInterpreterBridge extends OCamlParser {
  var output = ""
  var errors = ""

  def outputHandler(s: String) =
    { stdin: OutputStream =>
      val pw = new PrintWriter(new BufferedWriter(new OutputStreamWriter(stdin)), true)
      pw.println(s)
      pw.close()
    }

  def inputHandler(stdout: InputStream) =
    {
      output = scala.io.Source.fromInputStream(stdout).mkString
    }

  def errorHandler(stderr: InputStream) =
    {
      errors = scala.io.Source.fromInputStream(stderr).mkString
    }

  def runInterpreter(s: TopLevelInput): (String, String) = {
    output = ""
    errors = ""
    val p = Process(Seq("ocaml"))
    val pio = new ProcessIO(outputHandler(OCamlPrettyPrinter.pretty(s)),
      inputHandler, errorHandler)
    val ocaml = p.run(pio)
    ocaml.exitValue()
    (output, errors)
  }

  def runScript(s: String): (String, String) = {
    output = ""
    errors = ""
    val p = Process(Seq("ocaml", "-stdin"))
    val pio = new ProcessIO(outputHandler(s), inputHandler, errorHandler)
    val ocaml = p.run(pio)
    ocaml.exitValue()
    (output, errors)
  }
}
