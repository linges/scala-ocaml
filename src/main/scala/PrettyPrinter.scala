package scalaocaml

import org.kiama.output.PrettyPrinter

/**
 * Pretty printer for OCaml definitions and expressions.
 */
//TODO split into traits
object SyntaxPrettyPrinter extends PrettyPrinter {
  
  def pretty(t: Any): String = t match {
    case e: Type => super.pretty(showType(e))
    case e: Expr => super.pretty(showExpr(e))
    case e: TopLevel => super.pretty(showTopLevel(e))
    case e: Pattern => super.pretty(showPattern(e))
    case e: OException => super.pretty(showException(e))
    case e: ClassType => super.pretty(showClassType(e))
    case e: ClassExpr => super.pretty(showClassExpr(e))
    case e => pretty_any(e)
  }

  def showException(e: OException) : Doc = e match {
    case NewException(name) => "exception" <+> name
    case NewException(name, ts@ _*) => "exception" <+> name <+> "of" <+> catList(ts.map(showType), " *")
    case AlternateNameException(name , constr) => "exception" <+> name <+> "=" <+> constr
  }

  implicit def showClassExpr(ce: ClassExpr) : Doc = ce match {
    case ClassAscription(e,t)      => parens(e <+> ":" <+> t)
    case SimpleClassExpr(Nil, n) => n
    case SimpleClassExpr(targs, n) =>
      brackets(catList(targs.map(showType), comma)) <+> n
    case ClassObject(b)  => "object" <+> b <@> "end"
    case ClassLetIn(v, e) => group("let" <+> catList(v.map(showLetBinding), line <>"and") <@> "in" <> nest(line <> e))
    case ClassLetRecIn(v, e) => group("let rec" <+> catList(v.map(showLetBinding), line <>"and") <@> "in" <> nest(line <> e))
    case ClassApp(f,x)             => parens(f <+> x)
    case ClassApp(f,x, xs@ _*)     => parens(f <+> x <+> catList(xs.map(showArg), space))
    case ClassFun(args,e)  => parens("fun" <+> catList(args.map(showParameter), "") <+> "->" <+> e)
  }

  
  implicit def showClassBody(b: ClassBody) : Doc = b match {
    case ClassBody(cs, p, t) => (p.map{ pp =>
      parens( pp <> t.map{ tt => " :" <+> tt}.getOrElse(""))}.getOrElse(""): Doc) <> 
      nest(line <> lsep(cs.map(showClassField), line))
  }

  implicit def showClassField(cf: ClassField) : Doc = cf match {
    case Inherit(ce, as) => "inherit" <+> ce <> as.map(" as" <+> _).getOrElse("")
    case Val(name, e, muta, t) =>
      "val" <> (if(muta) " mutable" else "") <+> name <>
      t.map(" :" <+> _).getOrElse("") <+>
       "=" <+> e
    case VirtualVal(name, t, muta) =>
      "val" <> (if(muta) " mutable" else "") <+>
      "virtual" <+>
      name <+> ":" <+> t
    case Initializer(e) => "initializer" <+> e
    case Constraint(t1,t2) => "constraint" <+> t1 <+> "=" <+> t2 
    case Method(name, pars, e, t, priv) =>
      "method" <> (if(priv) " private" else "") <+>
      name <+> catList(pars.map(showParameter), "") <>
      t.map(" :" <+> _).getOrElse("") <+> 
      "=" <+> e
    case PolyMethod(name, pt, e, priv) =>
      "method" <> (if(priv) " private" else "") <+>
      name <+> ":" <+> pt <+> "=" <+> e
    case VirtualMethod(name, pt, priv) =>
      "method" <> (if(priv) " private" else "") <+>
      "virtual" <+> name <+> ":" <+> pt
  }

  implicit def showClassFieldSpec(cfs: ClassFieldSpec) : Doc = cfs match {
    case ClassConstraint(t1,t2) => "constraint" <+> t1 <+> "=" <+> t2 
    case MethodSpec(name, pt, priv, virt) =>
      "method" <> (if(priv) " private" else "") <>
      (if(virt) " virtual" else "") <+>
      name <+> ":" <+> pt
    case ValSpec(name, t, muta, virt) =>
      "val" <> (if(muta) " mutable" else "") <>
      (if(virt) " virtual" else "") <+>
      name <+> ":" <+> t
    case InheirtSpec(c) => "inherit" <+> c
  }

  implicit def showClassBodyType(cbt: ClassBodyType) : Doc = cbt match {
    case SimpleClassBodyType(Nil, n) => n
    case SimpleClassBodyType(targs, n) => 
      brackets(catList(targs.map(showType), comma)) <+> n
    case NormalClassBodyType(None, cfs@ _*) =>
      "object" <@> catList(cfs.map(showClassFieldSpec), "") <@> "end"
    case NormalClassBodyType(Some(t), cfs@ _*) =>
      "object" <+> parens(t) <@> nest(catList(cfs.map(showClassFieldSpec), "")) <@> "end"
  }

  implicit def showClassType(ct: ClassType) : Doc = ct match {
    case ClassType(cfs, b) => catList(cfs.map(showClassTypeFunctionArg), empty) <+> b
  }
  
  implicit def showClassTypeFunctionArg(arg: ClassTypeFunctionArg) : Doc = arg match {
    case ClassTypeFunctionArg(t, label, op) =>
        (if(op) "?" else "") <> label.map(value(_) <> ": ").getOrElse("") <> t <+> "->"
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
    case Binding(p,e)               => p <+> "=" <+> e
    case FunBinding(n, l, e, t, t2) => n <+> catList(l.map(showParameter), space) <>
        t.map(" :"<+> _).getOrElse("") <> t2.map(" :>"<+> _).getOrElse("") <+> "=" <+> e
  }

  implicit def showParameter(p: Parameter) : Doc = p match {
    case p: Pattern => showPattern(p)
    case LabeledPar(l, None) => "~" <> l
    case LabeledPar(l, Some(t)) => "~" <> l <+> ":" <+> t
    case LabeledParWithPattern(l, p) => "~" <> l <+> ":" <+> p
    case OptionalLabeledPar(l, None, None, None) => "?" <> l
    case OptionalLabeledPar(l, Some(p), None, None) => "?" <> l <+> ":" <+> p
    case OptionalLabeledPar(l, None, t, e) => "?" <> parens(l <> 
        t.map(" :"<+> _).getOrElse("") <> e.map(" ="<+> _).getOrElse("") )
    case OptionalLabeledPar(l, Some(p), t, e) => "?" <> l <+> ":" <+> parens(p <> 
        t.map(" :"<+> _).getOrElse("") <> e.map(" ="<+> _).getOrElse("") )
  }

  implicit def showTagSpec(t: TagSpec) : Doc = t match {
    case TagType(n, None) => "`" <> value(n)
    case TagType(n, Some(t)) => "`" <> value(n) <+> "of" <+> t
    case t : Type => showType(t)
  }

  implicit def showTagSpecFull(t: TagSpecFull) : Doc = t match {
    case TagTypeFull(n, None) => "`" <> value(n)
    case TagTypeFull(n, Some(t)) => "`" <> value(n) <+> "of" <+> catList(t.map(showType), " &")
    case t : Type => showType(t)
  }

  implicit def showType(e: Type) : Doc = e match {
    case TypeConstr(v)         => value(v)
    case TypeConstr(v, t)      => t <+> value(v)
    case TypeConstr(v, ts @ _*)  => list(ts.toList, "", showType) <+> value(v)
    case TypeIdent(v) => "'" <> value(v)
    case TUnderscore => "_"
    case FunctionType(t1,t2) => parens(t1 <+> "->" <+> t2)
    case LabeledFunctionType(l, t1, t2, false) =>  parens(value(l) <> ":" <+> t1 <+> "->" <+> t2)
    case LabeledFunctionType(l, t1, t2, true) =>  parens("?" <> value(l) <> ":" <+> t1 <+> "->" <+> t2)
    case TupleType(ts@ _*) =>   parens(catList(ts.map(showType), " * "))
    case TypeAlias(t, as) => parens(t <+> "as '" <> value(as))
    case HashType(n) => parens("#" <+> n)
    case HashType(n, t) => parens(t <+> "#" <+> n)
    case HashType(n, ts@ _*) => parens(list(ts.toList, "", showType) <+> "#" <+> n)
    case ObjectType(m, b) => enclose("<", catList(m.map{case (s,t) => s <+> ":" <+> t}, semi)
        <> (if(b) " ;.." else ""), ">")
    case ExactVariantType(ts@ _*) => brackets(catList(ts.map(showTagSpec), " |")) 
    case OpenVariantType(ts@ _*) => brackets(">" <> catList(ts.map(showTagSpec), " |")) 
    case CloseVariantType(ts, None) => brackets("<" <> catList(ts.map(showTagSpecFull), " |")) 
    case CloseVariantType(ts, Some(ls)) => brackets("<" <> catList(ts.map(showTagSpecFull), " |")
      <+> ">" <+> catList(ls.map{case s => "`" <> value(s)}, ""))
  }

  implicit def showPolyType(p : PolyType) : Doc = p match {
    case PolymorphType(ps, t) =>  catList(ps.map(s => "'" <> s), "") <+> dot <+> t 
    case t: Type => showType(t)
  }


  implicit def showExpr(e: Expr) : Doc = e match {
    case c: Constant          => showConstant(c)
    case InfixOp(a,o,b)       => parens(a <+> text(o) <+> b)
    case UnaryOp(o,a)         => parens(text(o) <+> a)
    case App(f,x)             => parens(f <+> x)
    case App(f,x, xs@ _*)     => parens(f <+> x <+> catList(xs.map(showArg), space))
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
    case Object(b)            => "object" <+> b <@> "end"
  }

  
  implicit def showArg(a: Argument) : Doc = a match {
    case e : Expr => showExpr(e)
    case LabeledArg(n, None) => "~" <> n
    case LabeledArg(n, Some(e)) => "~" <> n <> ":" <> e
    case OptionalLabeledArg(n, None) => "?" <> n
    case OptionalLabeledArg(n, Some(e)) => "?" <> n <> ":" <> e
  }


  implicit def showPatternMatching(b: PatternMatching) : Doc = b match {
    case Matching(p,e) => "|"<+> p <+> "->" <+> e
    case MatchingWithGuard(p,g,e) => "|"<+> p <+>"when" <+> g <+> "->" <+> e
  }

  implicit def showPattern(e: Pattern) : Doc = e match {
    case c: Constant => showConstant(c)
    case PVar(v) => value(v)
    case PTuple(l@ _*) => list(l.toList, "", showPattern)
    case PList(l@ _*) => if (l.isEmpty) "[]" else list(l.toList, "", showPattern, space<>"::")
    case PRecord(m) => enclose("{",  catList(m.map{
      case (s,t) => s <+> "=" <+> t }.toList, semi)
        ,"}")
    case RecordPunning(m@ _*) => enclose("{", catList(m.map(showName), semi) ,"}")
    case ConstrPattern(n) => n
    case ConstrPattern(n, p@ _*) => n <> parens(catList(p.map(showPattern),comma))
    case OrPattern(a,b) => a <+> "|" <+> b
    case FixSizeList(l) => brackets( catList(l.map(showPattern), semi) )
    case ArrayPattern(l@ _*)         => enclose("[|", catList(l.map(showPattern), semi), "|]")
    case Alias(p,n) => p <+> "as" <+> n
    case PTypeconstr(n) => "#" <> n
    case Underscore => "_"
  }

  implicit def showName(n: Name) : Doc = n match {
    case Name(n, Nil) => n
    case Name(n, l) => catList(l.map(string),dot) <> dot <> n
  }

  implicit def showTypeDef(e: TypeDef) : Doc = e match {
    case TypeDef(tvars, constr, t, tr, cs) => {
      showTypeParameters(tvars) <+> constr <>
      t.map(" =" <+> showType(_)).getOrElse("") <>
      tr.map(" =" <+> showTypeRepresentation(_)).getOrElse("") <@>
      catList(cs.map(showTypeConstraint), linebreak)
    }
  }
 
  def showTypeConstraint(cs: TypeConstraint) : Doc = cs match {
    case TypeConstraint(id, t) => "constraint '" <> id <+> "=" <+> t 
  }

  def showTypeRepresentation(tr: TypeRepresentation) : Doc = tr match {
    case TRecord(m@ _*)  => enclose("{",  catList(m.map(showRecordField), semi)
        ,"}")
    case ConstrDeclarations(l@ _*) => "|" <+> catList(l.map(showConstrDec), line <> "|" )
  }

  def showConstrDec(cd: ConstrDecl) : Doc = cd match {
    case ConstrDecl(s) => value(s)
    case ConstrDecl(s, ts@ _*) => value(s) <+> "of" <+> catList(ts.map(showType), " *")
  }

  implicit def showRecordField(rf: RecordField) : Doc = rf match {
    case ImmutableRecordField(s, t) => s <+> ":" <+> t
    case MutableRecordField(s, t) => "mutable" <+> s <+> ":" <+> t
  }

  def showTypeParameters(ts: List[TypeParameter]) : Doc = 
    if(ts.isEmpty) ""
    else if(ts.size == 1) showTypeParameter(ts(0))
    else list(ts.toList, "", showTypeParameter)

  def showTypeParameter(tp: TypeParameter) : Doc = tp match {
    case TypeParameter(id, Some(Covariant)) => "+" <> "'" <> id
    case TypeParameter(id, Some(Contravariant)) => "-" <> "'" <> id
    case TypeParameter(id, None) => "'" <> id
  }

  def showTopLevel(e: TopLevel) : Doc = e match {
    case Let(v@ _*) => group("let" <+> catList(v.map(showLetBinding), line <>"and"))
    case LetRec(v@ _*) => group("let rec" <+> catList(v.map(showLetBinding), line <>"and"))
    case TypeDefinition(v@ _*) => group("type" <+> catList(v.map(showTypeDef), line <>"and"))
  }
  def catList(l: Seq[Doc], sep: Doc): Doc = (group(lsep(l.toList, sep)))
}
