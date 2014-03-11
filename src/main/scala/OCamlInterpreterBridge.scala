package scalaocaml
import scala.sys.process._
import java.io._

object Test {
  def main(args: Array[String]) {
    val oib = new OCamlInterpreterBridge()
    val (output, errors) = oib.run("3+3", true )
    print(output)
    print(errors)
  }
}

class OCamlInterpreterBridge
{
  var output = ""
  var errors = ""

  def outputHandler(s: String, interactive: Boolean) =
      { stdin : OutputStream =>
        val pw =  new PrintWriter(new BufferedWriter(new OutputStreamWriter(stdin)), true)
        val input = s + (if (interactive) ";;" else "")
        pw.println(input)
        pw.close()
      }

  def inputHandler(stdout : InputStream) = 
  {
    output = scala.io.Source.fromInputStream(stdout).mkString
  }

  def errorHandler(stderr : InputStream) = 
  {
    errors = scala.io.Source.fromInputStream(stderr).mkString
  }

  def run(s: String, interactive: Boolean = true) : (String, String) = {
    output = ""
    errors = ""
    val p = if (interactive) Process(Seq("ocaml")) else Process(Seq("ocaml", "-stdin"))
    val pio = new ProcessIO(outputHandler(s, interactive), inputHandler, errorHandler)
    val ocaml = p.run(pio)
    ocaml.exitValue()
    (output, errors)
  }
}
