package scalaocaml




abstract class Identifier
case class ExtendedModuleName(moduleName: String, paths : List[ExtendedModulePath] = List()) extends Identifier
case class ExtendedModulePath(name: ExtendedModuleName, l: List[ExtendedModuleName] = List()) extends Identifier

case class ExtendedName(id: String, path: List[ExtendedModulePath] = List()) extends Identifier
case class Name(id: String, path: List[String]) extends Identifier
object Name
{
  def apply(s: String) : Name =
    {
      val path = s.split("\\.")
      Name(path.last, path.dropRight(1).toList)
    }
}




trait IdentifierPrettyPrinter {
  self : OCamlPrettyPrinter =>

  implicit def showIdentifier(n: Identifier) : Doc = n match {
    case Name(n, Nil) => n
    case Name(n, l) => catList(l.map(string),dot) <> dot <> n
    case ExtendedName(n, Nil) => n
    case ExtendedName(n, l) => catList(l.map(showIdentifier),dot) <> dot <+> n
    case ExtendedModulePath(n, Nil) => n
    case ExtendedModulePath(n, l) => catList(l.map(showIdentifier),dot) <> dot <> n
    case ExtendedModuleName(n, Nil) => n
    case ExtendedModuleName(n, l) => n <+> catList(l.map(x => parens(showIdentifier(x))), "")
  }
}



