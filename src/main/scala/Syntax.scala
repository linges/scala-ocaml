package scalaocaml


abstract class TopLevel
case class TypeDecl(tyvars: List[String], id: String, t: Type) extends TopLevel
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
  t2: Option[Type] = None) extends LetBinding //TODO: what is :> type

trait Pattern extends Parameter
case class PVar(v: String) extends Pattern
case class PTuple(l: Pattern*) extends Pattern 
case class PList(l: Pattern*) extends Pattern
case class PAscription(p: Pattern, t: Type) extends Pattern 
case class PTagName(n: String, t: Type) extends Pattern
case class PRecord(m: Map[String, Pattern]) extends Pattern
case class ConstrPattern(constr: Name, p: Pattern*) extends Pattern
case class OrPattern(a: Pattern, b: Pattern) extends Pattern
case class FixSizeList(l: List[Pattern]) extends Pattern
case object Underscore extends Pattern

trait PatternMatching 
case class MatchingWithGuard(p: Pattern, g: Expr, e: Expr) extends PatternMatching
case class Matching(p: Pattern, e: Expr) extends PatternMatching

trait Parameter 
case class LabeledArg(label: String, t:Option[Type] = None) extends Parameter
case class LabeledArgWithPattern(label: String, p: Pattern) extends Parameter
case class OptionalLabeledArg(label: String, p: Option[Pattern] = None, t: Option[Type] = None, default : Option[Expr] = None) extends Parameter

abstract class Type
case class TVar(v : String) extends Type
case class PolyVar(v : String) extends Type
case class TRecord(m: RecordField*) extends Type
abstract class RecordField
case class MutableRecordField(s: String,e: Type) extends RecordField
case class ImmutableRecordField(s: String,e: Type) extends RecordField

trait PolyTypeExpr

case class TVariant(v: Variant*) extends Type
abstract class Variant 
case class VariantField(name: String, t: Type) extends Variant
case class ConstantField(name: String) extends Variant

