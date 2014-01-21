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

case class Name(id: String, path: List[String])
object Name
{
  def apply(s: String) : Name =
    {
      val path = s.split("\\.")
      Name(path.last, path.dropRight(1).toList)
    }
}




sealed abstract class LetBinding
case class Binding(p: Pattern, e: Expr) extends LetBinding
case class FunBinding(name : String, args: List[Parameter], e: Expr, t: Option[Type] = None,
  t2: Option[Type] = None) extends LetBinding 

trait PatternMatching 
case class MatchingWithGuard(p: Pattern, g: Expr, e: Expr) extends PatternMatching
case class Matching(p: Pattern, e: Expr) extends PatternMatching

trait Parameter 
case class LabeledArg(label: String, t:Option[Type] = None) extends Parameter
case class LabeledArgWithPattern(label: String, p: Pattern) extends Parameter
case class OptionalLabeledArg(label: String, p: Option[Pattern] = None, t: Option[Type] = None, default : Option[Expr] = None) extends Parameter
