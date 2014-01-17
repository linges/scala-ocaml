package scalaocaml

import org.kiama.output.PrettyPrinter

/**
 * Pretty printer for OCaml definitions and expressions.
 */
object SyntaxPrettyPrinter extends PrettyPrinter {
  
  def pretty(t: Any): String = t match {
    case e: Type => super.pretty(showType(e))
    case e: Expr => super.pretty(showExpr(e))
    case e: TopLevel => super.pretty(showTopLevel(e))
    case e => pretty_any(e)
  }


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

  implicit def showLetBinding(lb: LetBinding) : Doc = lb match {
    case Binding(p,e) => p <+> "=" <+> e
    case FunBinding(n, l, e, t, t2) => n <+> catList(l.map(showParameter), space) <>
        t.map(" :"<+> _).getOrElse("") <> t2.map(" :>"<+> _).getOrElse("") <+> "=" <+> e
  }

  implicit def showParameter(p: Parameter) : Doc = p match {
    case p: Pattern => showPattern(p)
    case LabeledArg(l, None) => "~" <> l
    case LabeledArg(l, Some(t)) => "~" <> l <+> ":" <+> t
    case LabeledArgWithPattern(l, p) => "~" <> l <+> ":" <+> p
    case OptionalLabeledArg(l, None, None, None) => "?" <> l
    case OptionalLabeledArg(l, Some(p), None, None) => "?" <> l <+> ":" <+> p
    case OptionalLabeledArg(l, None, t, e) => "?" <> parens(l <> 
        t.map(" :"<+> _).getOrElse("") <> e.map(" ="<+> _).getOrElse("") )
    case OptionalLabeledArg(l, Some(p), t, e) => "?" <> l <+> ":" <+> parens(p <> 
        t.map(" :"<+> _).getOrElse("") <> e.map(" ="<+> _).getOrElse("") )
  }

  implicit def showType(e: Type) : Doc = e match {
    case TVar(v)         => value(v)
    case PolyVar(v)      => "'"<>value(v)
    case TRecord(m@ _*)  => enclose("{",  catList(m.map(showRecordField), semi)
        ,"}")
    case TVariant(l@ _*) => "|" <+> catList(l.map(showVariant), "|" )
  }

  implicit def showVariant(v: Variant) : Doc = v match {
    case VariantField(n,t) => n <+> "of" <+> t
    case ConstantField(n)  => n
  }


  implicit def showExpr(e: Expr) : Doc = e match {
    case c: Constant          => showConstant(c)
    case InfixOp(a,o,b)       => parens(a <+> text(o) <+> b)
    case UnaryOp(o,a)         => parens(text(o) <+> a)
    case App(f,x)             => parens(f <+> x)
    case App(f,x, xs@ _*)     => parens(f <+> x <+> catList(xs.map(showExpr), space))
    case Fun(args,e,None)     => parens("fun" <+> catList(args.map(showParameter), "") <+> "->" <+> e)
    case Fun(args,e,Some(g))  => parens("fun" <+> catList(args.map(showParameter), "") <+>
        "when"<+> g <+> "->" <+> e)
    case Function(ps@ _*)     => "function" <+> catList(ps.map(showPatternMatching), line)
    case Var(v)               => v
    case OList(l@ _*)         => brackets( catList(l.map(showExpr), semi) )
    case IfThenElse(c, t, e)  => "if" <+> showExpr(c) <+> "then" <>
      nest(line <> showExpr(t)) <@> "else" <> nest(line <> showExpr(e))
    case IfThen(c, t)         => "if" <+> showExpr(c) <+> "then" <>
      nest(line <> showExpr(t))
    case LetIn(v, e)          => group("let" <+> catList(v.map(showLetBinding), line <>"and") <@> "in" <> nest(line <> e))
    case LetRecIn(v, e)       => group("let rec" <+> catList(v.map(showLetBinding), line <>"and") <@> "in" <> nest(line <> e))
    case Tuple(l@ _*)         => list(l.toList, "", showExpr)
    case Constr(n)            => n
    case Constr(n, l@ _*)     => n <> list(l.toList, "", showExpr)
    case Match(e, bs@ _*)     => "match" <+> e <+> "with" <>
      nest(line <> bs.map(showPatternMatching).reduce(_ <@> _))
    case Try(e, bs@ _*)       => "try" <+> e <+> "with" <>
      nest(line <> bs.map(showPatternMatching).reduce(_ <@> _))
    case Record(m)            => enclose("{",  catList(m.map{
      case (s,t)              => s <+> "=" <+> t }.toList, semi)
        ,"}")
    case RecordCopy(e,m)      => enclose("{", e <+> "with" <+>
        catList(m.map{
          case (s,t)          => s <+> "=" <+> t }.toList, semi)
        ,"}")
    case RecordAccess(e,s)    => e <> dot <> s 
    case RecordUpdate(r,s,e)  => r <> dot <> s <+> "<-" <+> e
    case ForTo(i,from,to,e)   => "for" <+> i <+> "=" <+> from <+> "to" <+> to <+> "do" <>
      nest(line <> e) <@> "done"
    case ForDown(i,from,to,e) => "for" <+> i <+> "=" <+> from <+> "downto" <+> to <+> "do" <>
      nest(line <> e) <@> "done"
    case While(e,b)           => "while" <+> e <+> "do" <> nest(line <> b) <@> "done"
    case Sequence(l@ _*)      => parens(catList(l.map(showExpr), semi))
    case Ascription(e,t)      => parens(e<+>":"<+>t)
    case Array(l@ _*)         => enclose("[|", catList(l.map(showExpr), semi), "|]")
    case ArrayAccess(a,e)     => a <> dot <> parens(e) 
    case ArrayUpdate(a,i,e)   => a <> dot <> parens(i) <+> "<-" <+> e
    case New(n)               => "new" <+> n
    case CharOf(s,i)          => s <> dot <> brackets(i)
    case UpdateString(s,i,c)  => s <> dot <> brackets(i) <+> "<-" <+> c
    case BeginEnd(e)          => "begin" <@> e <@> "end"
    case MethodCall(e, s)     => e <+> "#" <+> s
    case InstVar(s)           => value(s)
    case AssignInstVar(s, e)  => value(s) <+> "<-" <+> e
    case Object(b)            => "object" <@> b <@> "end"

  }

  implicit def showClassBody(b: ClassBody) : Doc = ""

  implicit def showPatternMatching(b: PatternMatching) : Doc = b match {
    case Matching(p,e) => "|"<+> p <+> "->" <+> e
    case MatchingWithGuard(p,g,e) => "|"<+> p <+>"when" <+> g <+> "->" <+> e
  }
  implicit def showRecordField(rf: RecordField) : Doc = rf match {
    case ImmutableRecordField(s, t) => s <+> ":" <+> t
    case MutableRecordField(s, t) => "mutable" <+> s <+> ":" <+> t
  }

  implicit def showPattern(e: Pattern) : Doc = e match {
    case c: Constant => showConstant(c)
    case PVar(v) => value(v)
    case PTuple(l@ _*) => list(l.toList, "", showPattern)
    case PList(l@ _*) => if (l.isEmpty) "[]" else list(l.toList, "", showPattern, space<>"::")
    case PRecord(m) => enclose("{",  catList(m.map{
      case (s,t) => s <+> "=" <+> t }.toList, semi)
        ,"}")
    case ConstrPattern(n) => n
    case ConstrPattern(n, p@ _*) => n <> parens(catList(p.map(showPattern),comma))
    case OrPattern(a,b) => a <+> "|" <+> b
    case FixSizeList(l) => brackets( catList(l.map(showPattern), semi) )
    case Underscore => "_"
  }

  implicit def showName(n: Name) : Doc = n match {
    case Name(n, Nil) => n
    case Name(n, l) => catList(l.map(string),dot) <> dot <> n
  }

  def showTopLevel(e: TopLevel) : Doc = e match {
    case Let(v@ _*) => group("let" <+> catList(v.map(showLetBinding), line <>"and"))
    case LetRec(v@ _*) => group("let rec" <+> catList(v.map(showLetBinding), line <>"and"))
    case TypeDecl(Nil, id, t) =>  group("type" <+> id <+> "=" <> nest(line <> t))
    case TypeDecl(tvs, id, t) =>  group("type" <+> parens(catList(tvs.map("'"<>_), comma))
        <+> id <+> "=" <> nest(line <> t))
  }
  def catList(l: Seq[Doc], sep: Doc): Doc = (group(lsep(l.toList, sep)))
}
