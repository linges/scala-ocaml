package scalaocaml


abstract class TopLevel

/**
  * Type definitions are introduced by the type keyword, 
  * and consist in one or several simple definitions, 
  * possibly mutually recursive, separated by the and keyword. 
  * Each simple definition defines one type constructor.
  */
case class TypeDefinition(td: TypeDef*) extends TopLevel


case class Let(lb: LetBinding*) extends TopLevel
case class LetRec(lb: LetBinding*) extends TopLevel

//TODO check extended-module-name
case class Name(id: String, path: List[String])
object Name
{
  def apply(s: String) : Name =
    {
      val path = s.split("\\.")
      Name(path.last, path.dropRight(1).toList)
    }
}





/**
  * Exception definitions add new constructors to the built-in
  * variant type exn of exception values. The constructors are declared 
  * as for a definition of a variant type.
  * 
  * The form exception constr-name  [of typexpr  {* typexpr}] generates a new exception, 
  * distinct from all other exceptions in the system. 
  * The form exception constr-name =  constr gives an alternate 
  * name to an existing exception. 
  */
abstract class OException
case class NewException(name: String, ts: Type*) extends OException
case class AlternateNameException(name : String, constr: Name) extends OException
