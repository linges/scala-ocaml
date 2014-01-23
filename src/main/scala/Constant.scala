package scalaocaml

/**
  * The syntactic class of constants comprises literals from the four base types
  * (integers, floating-point numbers, characters, character strings), 
  * and constant constructors from both normal and polymorphic variants, 
  * as well as the special constants false, true, (), [], and [||], 
  * which behave like constant constructors, and begin end, which is equivalent to (). 
  * A pattern consisting in a constant matches the values that are equal to this constant.
  */
sealed abstract class Constant extends Expr with Pattern 
case class OInt(n: Int) extends Constant
case class OFloat(n: Double) extends Constant
sealed abstract class OBoolean extends Constant
case object True extends OBoolean
case object False extends OBoolean
case class OChar(c: Char) extends Constant 
case class OString(s: String) extends Constant
case object Unit extends Constant
case object EmptyList extends Constant 
case object EmptyArray extends Constant 
case object EmptyBeginEnd extends Constant
case class TagName(s: String) extends Constant 

trait ConstantPrettyPrinter {
  self: OCamlPrettyPrinter =>

  implicit def showConstant(c: Constant) : Doc = c match {
    case OInt(v)       => value(v)
    case OFloat(v)     => value(v)
    case OChar(c)      => "'" <> value(c) <> "'"
    case OString(s)    => '"' <> string(s) <> '"'
    case TagName(s)    => "`" <> s
    case True          => "true"
    case False         => "false"
    case Unit          => "()"
    case EmptyList     => "[]"
    case EmptyArray    => "[||]"
    case EmptyBeginEnd => "begin end"
  }
}
