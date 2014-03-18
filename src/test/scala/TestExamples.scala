
package scalaocaml.test
import org.scalatest.FunSuite
import scala.io.Source
import scalaocaml._

trait TestExamples extends FunSuite {
  def compare(result: Any, expect: String)
  def compareType(result: Type, expect: String)
  def compareExpr(result: Expr, expect: String)
  def compareClassExpr(result: ClassExpr, expect: String)
  def compareClassType(result: ClassType, expect: String)
  def compareDef(result: Definition, expect: String)

  implicit def intToOCaml(i: Int) = OInt(i)
  implicit def stringToVar(s: String) = Var(Name(s))
  implicit def tupleToRecordField(t: Tuple2[String, Type]) =
    ImmutableRecordField(t._1, t._2)

  val int = TypeConstr(ExtendedName("int"))
  val string = TypeConstr(ExtendedName("string"))
  val mt = MTVar(ExtendedName("mt"))

  /**
    * Expressions and Pattern
    */
  test("int") {
    val result = OInt(2)
    val expect = "2"
    compareExpr(result, expect)
  }

  test("float") {
    val result = OFloat(2.3)
    val expect = "2.3"
    compareExpr(result, expect)
  }

  test("string") {
    val result = OString("string")
    val expect = """"string""""
    compareExpr(result, expect)
  }

  test("char") {
    val result = OChar('c')
    val expect = """'c'"""
    compareExpr(result, expect)
  }

  test("unit") {
    val result = Unit
    val expect = "()"
    compareExpr(result, expect)
  }
  test("tagname") {
    val result = TagName("Foo")
    val expect = "`Foo"
    compareExpr(result, expect)
  }

  test("begin end constant") {
    val result = EmptyBeginEnd
    val expect = "begin end"
    compareExpr(result, expect)
  }

  test("int operator") {
    val result = InfixOp(OInt(2), "-", OInt(3))
    val expect = "(2 - 3)"
    compareExpr(result, expect)
  }

  test("prefix binary operator") {
    val result = App(App("(+)", OInt(2)), OInt(3))
    val expect = "(((+) 2) 3)"
    compareExpr(result, expect)
  }

  test("prefix minus") {
    val result = UnaryOp("-", "x")
    val expect = "(- x)"
    compareExpr(result, expect)
  }

  test("prefix op") {
    val result = UnaryOp("!", "x")
    val expect = "(! x)"
    compareExpr(result, expect)
  }

  test("float operator") {
    val result = InfixOp(OFloat(2.2), "+.", OFloat(3.3))
    val expect = "(2.2 +. 3.3)"
    compareExpr(result, expect)
  }

  test("simple let") {
    val result = Let(Binding(PVar("x"), OInt(3)))
    val expect = "let x = 3"
    compareDef(result, expect)
  }
  test("fun let 1") {
    val result = Let(FunBinding("square", List(PVar("x")), "x"))
    val expect = "let square x = x"
    compareDef(result, expect)
  }

  test("fun let 2") {
    val result = Let(FunBinding("ratio", List(PVar("x"), PVar("y")),
      InfixOp(
        App("Float.of_int", "x"), "/.",
        App("Float.of_int", "y"))))
    val expect = """let ratio x y = 
                      ((Float.of_int x) /. (Float.of_int y))"""
    compareDef(result, expect)
  }

  test("application") {
    val result = App(App("ratio", OInt(4)), OInt(7))
    val expect = """((ratio 4) 7)"""
    compareExpr(result, expect)
  }

  test("if then else") {
    val result = IfThenElse(True, OInt(1), OInt(0))
    val expect = """if true then 1 else 0"""
    compareExpr(result, expect)
  }
  test("if then + sequence") {
    val result = Sequence(IfThen(True, OInt(1)), OInt(2))
    val expect = """if true then 1; 2"""
    compareExpr(result, expect)
  }
  test("if then") {
    val result = IfThen(True, OInt(1))
    val expect = """if true then 1"""
    compareExpr(result, expect)
  }
  test("tuple") {
    val result = Tuple(OInt(3), OInt(2))
    val expect = """(3, 2)"""
    compareExpr(result, expect)
  }
  test("let tuple pattern") {
    val result = Let(FunBinding("di",
      List(PTuple(PVar("x1"), PVar("y1")),
        PTuple(PVar("x2"), PVar("y2"))),
      App("sqrt",
        InfixOp(
          InfixOp("x1", "-.", "x2"), "**", OFloat(2.0)))))
    val expect = """
    let di (x1, y1) (x2, y2) = 
        (sqrt ((x1 -. x2) ** 2.0))
    """
    compareDef(result, expect)
  }

  test("application constr") {
    val result = App("f", Constr(Name("None"))) 
    val expect = "(f None)" 
    compareExpr(result, expect)
  }

  test("list") {
    val result = OList(OString("OCaml"), OString("Perl"), OString("C"))
    val expect = """["OCaml"; "Perl"; "C"]"""
    compareExpr(result, expect)
  }

  test("list ::") {
    val result = InfixOp(OInt(1), "::", InfixOp(OInt(2), "::", EmptyList))
    val expect = """(1 :: (2 :: []))"""
    compareExpr(result, expect)
  }

  test("list @ list") {
    val result = InfixOp(OList(1, 2, 3), "@", OList(4, 5, 6))
    val expect = """([1; 2; 3] @ [4; 5; 6])"""
    compareExpr(result, expect)
  }

  test("let list pattern") {
    val result = Let(FunBinding("my_favorite_language", List(
      PList(PVar("my_favorite"), PVar("the_rest"))), "my_favorite"))
    val expect = """let my_favorite_language (my_favorite :: the_rest) =
     my_favorite"""
    compareDef(result, expect)
  }

  test("match list pattern") {
    val result = Match("languages",
      Matching(PList(PVar("first"), PVar("snd"), PVar("the_rest")), "first"),
      Matching(EmptyList, OString("OCaml")))
    val expect = """
    match languages with
    | (first :: snd :: the_rest) -> first
    | [] -> "OCaml""""
    compareExpr(result, expect)
  }

  test("options") {
    val result = Let(FunBinding("divide", List(PVar("x"), PVar("y")),
      IfThenElse(InfixOp("y", "=", OInt(0)),
        Constr(Name("None")),
        Constr(Name("Some"), InfixOp("x", "/", "y")))))
    val expect = """
    let divide x y = 
    if (y = 0) then None else Some((x / y))
    """
    compareDef(result, expect)
  }
  test("let in") {
    val result = LetIn(List(Binding(PVar("x"), OInt(7))), InfixOp("x", "+", "x"))
    val expect = """
    let x = 7 in
        (x + x)
    """
    compareExpr(result, expect)
  }

  test("nested let rec in") {
    val result = LetRecIn(List(Binding(PVar("x"), OInt(7))),
      LetRecIn(List(Binding(PVar("y"), InfixOp("x", "*", "x"))), InfixOp("x", "+", "y")))
    val expect = """
    let rec x = 7 in
    let rec y = (x * x) in
    (x + y)
    """
    compareExpr(result, expect)
  }

  test("record") {
    val result = Record(Map(Name("x") -> OFloat(3.0), Name("y") -> UnaryOp("-", OFloat(4.0))))
    val expect = """
    {x = 3.0; y = (- 4.0)}
    """
    compareExpr(result, expect)
  }

  test("let record pattern") {
    val result = Let(FunBinding("mag",
      List(PRecord(Map(Name("x") -> Some(PVar("x_pos")), Name("y") -> Some(PVar("y_pos"))))),
      "y_pos"))
    val expect = """
    let mag {x = x_pos; y = y_pos} = y_pos
    """
    compareDef(result, expect)
  }

  test("let record pattern field punning") {
    val result = Let(FunBinding("mag",
      List(PRecord(Map(Name("x") -> None, Name("y") -> Some(PVar("y_pos"))))),
      "y_pos"))
    val expect = """
    let mag {x; y = y_pos} = y_pos
    """
    compareDef(result, expect)
  }

  test("record access") {
    val result = RecordAccess("r", Name("x"))
    val expect = """r.x"""
    compareExpr(result, expect)
  }
 
  test("record copy") {
    val result = RecordCopy("r", Map(Name("x") -> OInt(2),
         Name("y") -> OInt(3)))
    val expect = """{r with x = 2; y = 3}"""
    compareExpr(result, expect)
  }

  test("match variant") {
    val result = Match("fb",
      Matching(ConstrPattern(Name("Foo"), PVar("x")), "x"),
      Matching(ConstrPattern(Name("Bar"), PVar("x")), "x"))
    val expect = """
    match fb with
    | Foo(x) -> x
    | Bar(x) -> x
    """
    compareExpr(result, expect)
  }

  test("array") {
    val result = OArray(OInt(1), OInt(2), OInt(3))
    val expect = """
    [|1; 2; 3|]
    """
    compareExpr(result, expect)
  }

  test("array access") {
    val result = ArrayAccess("numbers", OInt(2))
    val expect = """
    numbers.(2)
    """
    compareExpr(result, expect)
  }

  test("array access 2") {
    val result = ArrayAccess(ArrayAccess("numbers", OInt(2)), OInt(3))
    val expect = """
    numbers.(2).(3)
    """
    compareExpr(result, expect)
  }

  test("array update") {
    val result = ArrayUpdate("numbers", OInt(2), OInt(4))
    val expect = """
    numbers.(2) <- 4
    """
    compareExpr(result, expect)
  }

  test("array/record access") {
    val result = RecordAccess(ArrayAccess("numbers", OInt(2)), Name("a"))
    val expect = """
    numbers.(2).a
    """
    compareExpr(result, expect)
  }

  test("array/record access 2") {
    val result = ArrayAccess(RecordAccess("numbers", Name("a")), OInt(2))
    val expect = """
    numbers.a.(2)
    """
    compareExpr(result, expect)
  }

  test("unit as pattern") {
    val result = Let(FunBinding("create", List(Unit), Record(Map(Name("sum") -> OFloat(0.0)))))
    val expect = """let create () = {sum = 0.0}
    """
    compareDef(result, expect)
  }

 
  test("update mutable record and sequence") {
    val result = Sequence(RecordUpdate("r", Name("samples"), OInt(1)),
      RecordUpdate("r", Name("sum"), "x"))
    val expect = """
     r.samples <- 1;
     r.sum     <- x
    """
    compareExpr(result, expect)
  }
  test("let rec") {
    val result = LetRec(FunBinding("f", List(PVar("x")), App("f", "x")))
    val expect = """let rec f x = (f x)"""
    compareDef(result, expect)
  }

  test("anonymous functions") {
    val result = Fun(List(
      PVar("x"),
      LabeledPar("y", Some(int)),
      LabeledParWithPattern("z", EmptyArray),
      OptionalLabeledPar("l", None, Some(TypeConstr(ExtendedName("option"), int)), 
                           Some(Constr(Name("Some"), OInt(0)))),
      OptionalLabeledPar("t", Some(OInt(0)), Some(int), Some(OInt(0)))),
      "x",
      Some("y"))
    val expect = """
    (fun x (~y : int) ~z : [||] ?(l : int option = Some(0)) ?t : (0 : int = 0) when y  -> x)
    """
    compareExpr(result, expect)
  }


  test("match") {
    val result = Match("list",
      Matching(EmptyList , Constr(Name("None"))),
      Matching(FixSizeList(List(PVar("_"))), Constr(Name("None"))))
    val expect = """match list with
     | []  -> None
     | [_] -> None"""
    compareExpr(result, expect)
  }

  test("or pattern and underscore pattern") {
    val result = Match("list",
      Matching(OrPattern(EmptyList, FixSizeList(List(PVar("_")))), Constr(Name("None"))))
    val expect = """match list with
    | [] | [_] -> None"""
    compareExpr(result, expect)
  }

  test("let operator ") {
    val result = Let(FunBinding("(|>)", List(PVar("x"), PVar("f")), App("f", "x")))
    val expect = """ let (|>) x f = (f x)"""
    compareDef(result, expect)
  }

  test("for to") {
    val result = ForTo("i", OInt(1), OInt(10),
      App("print_int", "i"))
    val expect = """
    for i = 1 to 10 do (print_int i) done"""
    compareExpr(result, expect)
  }
  test("for downto") {
    val result = ForDown("i", OInt(1), OInt(10),
      App("print_int", "i"))
    val expect = """
    for i = 1 downto 10 do (print_int i) done"""
    compareExpr(result, expect)
  }

  test("while") {
    val result = While(True, "x")
    val expect = """
    while true
    do x done
    """
    compareExpr(result, expect)
  }

  test("ascription ") {
    val result = Ascription(OInt(1), int)
    val expect = """ (1 : int)"""
    compareExpr(result, expect)
  }
  test("function with pattern matching and guard") {
    val result = Function(
      MatchingWithGuard(ConstrPattern(Name("Pair"),PTuple( PVar("x"), PVar("y"))), True, "x"),
      Matching(PVar("_"), OInt(0)))
    val expect = """ function
     | Pair((x, y)) when true -> x
     | _ -> 0"""
    compareExpr(result, expect)
  }

  test("let labeled arguments") {
    val result = Let(FunBinding("ratio", List(
      LabeledPar("num"),
      LabeledPar("denom")), "num"))
    val expect = """ 
    let ratio ~num ~denom = num 
    """
    compareDef(result, expect)
  }

  test("try") {
    val result = Try(
      App(App("List.assoc", "digit"), OList(OInt(2))),
      Matching(ConstrPattern(Name("Not_found")), OString("not found")))
    val expect = """ 
    try
      ((List.assoc digit) [2])
    with |  Not_found -> "not found"
    """
    compareExpr(result, expect)
  }
  test("constr") {
    val result = Constr(Name("None")) 
    val expect = """ 
    None 
    """
    compareExpr(result, expect)
  }

  test("char of") {
    val result = CharOf(OString("foo"), OInt(2))
    val expect = """ 
    "foo".[2]
    """
    compareExpr(result, expect)
  }

  test("update string") {
    val result = UpdateString(OString("foo"), OInt(2), OChar('c'))
    val expect = """ 
    "foo".[2] <- 'c'
    """
    compareExpr(result, expect)
  }

  test("begin end") {
    val result = BeginEnd(OInt(2)) 
    val expect = """ 
    begin 2 end
    """
    compareExpr(result, expect)
  }

  test("new") {
    val result = New(Name("Foo.bar"))
    val expect = """ 
    new Foo.bar
    """
    compareExpr(result, expect)
  }

  test("method call") {
    val result = App(MethodCall(RecordAccess(Record(Map(Name("a")-> "foo")), Name("a")), "fn"), OInt(3))
    val expect = """ 
    ({a = foo}.a # fn 3)
    """
    compareExpr(result, expect)
  }

  test("method call 2") {

    val result = MethodCall("a", "fn")
    val expect = """ 
    a # fn 
    """
    compareExpr(result, expect)
  }

  test("instance variable") {
    val result = Var(Name("foo"))
    val expect = """ 
    foo
    """
    compareExpr(result, expect)
  }

  test("assign instance variable") {
    val result = AssignInstVar("foo", OInt(2))
    val expect = """ 
    foo <- 2
    """
    compareExpr(result, expect)
  }

  test("array and alias pattern") {
    val result = Let(FunBinding("f",List(Alias(ArrayPattern(OInt(1), OInt(2)),"a")),OInt(3)))
    val expect = """ 
    let f [|1; 2|] as a = 3
    """
    compareDef(result, expect)
  }

  test("poly variant pattern") {
    val result = Let(FunBinding("f",List(PolyVariantPattern("A", OInt(3))),OInt(3)))
    val expect = """ 
    let f `A 3 = 3 
    """
    compareDef(result, expect)
  }

  test("labeled arguments application"){
    val result = App(App(App("r",LabeledArg("num", Some(OInt(3)))),
      LabeledArg("denom")), OptionalLabeledArg("f"))
    val expect = """ 
    (((r ~num:3) ~denom) ?f)
    """
    compareExpr(result, expect)
  } 

  test("tagged expr"){
    val result = TaggedExpr("foo", OInt(7))
    val expect = """ 
    (`foo 7)
    """
    compareExpr(result, expect)
  }

  test("coercion"){
    val result = Coercion(Coercion("f", Some(int), int), None, int) 
    val expect = """ 
    ((f : int :> int) :> int)
    """
    compareExpr(result, expect)
  }
  /**
    * Types and Typedefinition
    */
  test("object type"){
    val result = ObjectType(List("foo" -> int,
                                 "bar" -> PolymorphType(List("a", "b"), TypeConstr(ExtendedName("test")))) , true)
    val expect = """ 
    <foo : int; bar : 'a 'b . test ;..>
    """
    compareType(result, expect)
  }


  test("object type 2"){
    val result = ObjectType(List("foo" -> int,
                                 "bar" -> PolymorphType(List("a", "b"), TypeConstr(ExtendedName("test")))) , false)
    val expect = """ 
    <foo : int; bar : 'a 'b . test>
    """
    compareType(result, expect)
  }

  test("object type 3"){
    val result = ObjectType(Nil, false)
    val expect = """ 
    <>
    """
    compareType(result, expect)
  }

  test("object type 4"){
    val result = ObjectType(Nil, true)
    val expect = """ 
    <..>
    """
    compareType(result, expect)
  }

  test("exact variant type"){
    val result = ExactVariantType(None, TagType("Foo", Some(TypeConstr(ExtendedName("a")))), TypeConstr(ExtendedName("bar")))
    val expect = """ 
    [`Foo of a | bar]
    """
    compareType(result, expect)
  }

  test("exact variant type 2"){
    val result = ExactVariantType(Some(int), TagType("Foo", Some(TypeConstr(ExtendedName("a")))), TypeConstr(ExtendedName("bar")))
    val expect = """ 
    [int | `Foo of a | bar]
    """
    compareType(result, expect)
  }

  test("open variant type"){
    val result = OpenVariantType(TagType("Foo", Some(TypeConstr(ExtendedName("a")))), TypeConstr(ExtendedName("bar")))
    val expect = """ 
    [>`Foo of a | bar]
    """
    compareType(result, expect)
  }

  test("close variant type"){
    val result = CloseVariantType(List(
      TagTypeFull("Foo", Some(List(TypeConstr(ExtendedName("a")),TypeConstr(ExtendedName("b"))))), TypeConstr(ExtendedName("bar"))),
      Some(List("C", "D"))
    )
    val expect = """ 
    [<`Foo of a & b | bar > `C `D]
    """
    compareType(result, expect)
  }

  test("close variant type 2"){
    val result = CloseVariantType(List(
      TagTypeFull("Foo", None))
    )
    val expect = """ 
    [<`Foo]
    """
    compareType(result, expect)
  }

  test("Typeconstr pattern") {
    val result = PTypeconstr(ExtendedName("foo")) 
    val expect = """ 
    #foo
    """
    compare(result, expect)
  }

  test("type ident"){
    val result = TypeIdent("a") 
    val expect = """ 
    'a
    """
    compareType(result, expect)
  }

  test("type underscore"){
    val result = TypeConstr(ExtendedName("_"))
    val expect = """ 
    _
    """
    compareType(result, expect)
  }

  test("function type"){
    val result = FunctionType(FunctionType(LabeledFunctionParameter("foo", int), string),
        FunctionType(LabeledFunctionParameter("bar", int, true), string))
    val expect = """ 
    ((foo: int -> string) -> (?bar: int -> string))  
    """
    compareType(result, expect)
  }

  test("function type 2"){
    val result = FunctionType(int, FunctionType(int, int))
    val expect = """ 
    (int -> (int -> int))
    """
    compareType(result, expect)
  }

  test("tuple type"){
    val result = TupleType(int,
      int,
      int)
    val expect = """ 
    (int * int * int)
    """
    compareType(result, expect)
  }

  test("type constr int"){
    val result = int
    val expect = """ 
    int
    """
    compareType(result, expect)
  }

  test("type constr pair"){
    val result = TypeConstr(ExtendedName("pair"),
      int,
      int)
    val expect = """ 
    (int, int) pair
    """
    compareType(result, expect)
  }

  test("type constr option"){
    val result = TypeConstr(ExtendedName("option"),
      int)
    val expect = """ 
    int option 
    """
    compareType(result, expect)
  }

  test("type constr option 2"){
    val result = TypeConstr(ExtendedName("option"),
      TypeConstr(ExtendedName("option"),
      int))
    val expect = """ 
    int option option
    """
    compareType(result, expect)
  }

  test("type alias"){
    val result = TypeAlias(TypeConstr(ExtendedName("pair"),
      int,
      int), "test")
    val expect = """ 
    ((int, int) pair as 'test)
    """
    compareType(result, expect)
  }

  test("type alias 2"){
    val result = TypeAlias(TypeConstr(ExtendedName("option"),
      int), "test")
    val expect = """ 
    (int option as 'test)
    """
    compareType(result, expect)
  }

  test("# class"){
    val result = HashType(Name("classp")) 
    val expect = """ 
    (# classp)
    """
    compareType(result, expect)
  }

  test("int # class"){
    val result = HashType(Name("classp"), int) 
    val expect = """ 
    (int # classp)
    """
    compareType(result, expect)
  }

  test("(int, int) # class"){
    val result = HashType(Name("classp"), int, int) 
    val expect = """ 
    ((int, int) # classp)
    """
    compareType(result, expect)
  }

  test("record type with constraints") {
    val result = TypeDefinition(TypeDef(List(), "point2d", None,
      Some(TRecord("x" -> TypeConstr(ExtendedName("float")), "y" -> TypeConstr(ExtendedName("float")))),
    List(TypeConstraint("a", TypeConstr(ExtendedName("float"))), TypeConstraint("x", TypeConstr(ExtendedName("b"))))))
    val expect = """
    type point2d = {x : float; y : float}
    constraint 'a = float
    constraint 'x = b
    """
    compareDef(result, expect)
  }

  test("type with constructor and type-equation") {
    val result = TypeDefinition(TypeDef(List(), "foobar",
      Some(int), 
      Some(ConstrDeclarations(ConstrDecl("Foo", TupleType(int, int)), 
        ConstrDecl("Bar")))))
    val expect = """
    type foobar = int = | Foo of (int * int) | Bar  
    """
    compareDef(result, expect)
  }

  test("mutable record type") {
    val result = TypeDefinition(TypeDef(
      List(TypeParameter("a",Some(Covariant))),
      "r", None,
      Some(
      TRecord(MutableRecordField("sum", TypeConstr(ExtendedName("float"))),
        MutableRecordField("samples", int)))))
    val expect = """
    type +'a r =
    {mutable sum : float;
      mutable samples : int}
    """
    compareDef(result, expect)
  }

  test("unit type") {
    val result = TypeConstr(ExtendedName("unit"))
    val expect = "unit"
    compareType(result, expect)
  }
 
  test("polymorphic type") {
    val result = TypeDefinition(TypeDef( 
      List(TypeParameter("a"), TypeParameter("b")), "ref", None,
      Some(TRecord(MutableRecordField("contents", TypeIdent("a"))))))
    val expect = """type ('a, 'b) ref = {mutable contents : 'a}"""
    compareDef(result, expect)
  }
  /**
    * Exceptions
    */
  test("exception definition"){
    val result = NewException("Foo", TupleType(int, string))
    val expect = """ 
    exception Foo of (int * string)
    """
    compareDef(result, expect)
  }

  test("exception definition 2"){
    val result = NewException("Foo")
    val expect = """ 
    exception Foo 
    """
    compareDef(result, expect)
  }

  test("alternate name for exception"){
    val result = AlternateNameException("Bar", Name("Foo"))
    val expect = """ 
    exception Bar = Foo
    """
    compareDef(result, expect)
  }

  /**
    * Classes
    */


  test("class type") {
    val result = ClassType(List(),
      NormalClassBodyType(Some(int),
        ValSpec("a", int, true, true),
        ValSpec("b", int, false, true),
        ValSpec("c", int, false, false),
        MethodSpec("d", int, true, true),
        MethodSpec("e", int, false, true),
        MethodSpec("f", int),
        InheritSpec(ClassType(List(),SimpleClassBodyType(List(),Name("foo")))),
        ClassConstraint(int, int)
      ))
    val expect = """
    object (int)
      val mutable virtual a : int
      val virtual b : int
      val c : int
      method private virtual d : int
      method virtual e : int
      method f : int
      inherit foo
      constraint int = int
    end"""
    compareClassType(result, expect)
  }

  test("simple class type") {
    val result = ClassType(List(
      ClassTypeFunctionArg(int, Some("foo"), true),
      ClassTypeFunctionArg(string)),
      SimpleClassBodyType(List(int,string), Name("name")))
    val expect = """ 
    ?foo: int ->
    string -> 
    [int, string] name
    """
    compareClassType(result, expect)
  }

  test("simple class type 2") {
    val result = ClassType(List(
      ClassTypeFunctionArg(int, Some("foo"), true),
      ClassTypeFunctionArg(string, Some("bar"))),
      SimpleClassBodyType(List(int,string), Name("name")))
    val expect = """ 
    ?foo: int ->
    bar: string -> 
    [int, string] name
    """
    compareClassType(result, expect)
  }

  test("simple class expression") {
    val result = SimpleClassExpr(List(int, string), Name("foo"))
    val expect = """
    [int, string] foo
    """
    compareClassExpr(result, expect)
  }

  test("class ascription") {
    val result = ClassAscription(SimpleClassExpr(List(), Name("foo")),
      ClassType(List(),
      SimpleClassBodyType(List(), Name("foo"))) )
    val expect = """
    (foo : foo) 
    """
    compareClassExpr(result, expect)
  }

  test("class fun and app") {
    val foo = SimpleClassExpr(List(), Name("foo"))
    val result = ClassApp(ClassApp(
      ClassFun(List(PVar("a"),PVar("b")), foo),
      "x"), "y")
    val expect = """
    (((fun a b -> foo) x) y)
    """
    compareClassExpr(result, expect)
  }

  test("class let") {
    val foo = SimpleClassExpr(List(), Name("foo"))
    val result = ClassLetIn(List(Binding(PVar("x"), OInt(7))), foo)
    val expect = """
    let x = 7
    in foo
    """
    compareClassExpr(result, expect)
  }

  test("class let rec") {
    val foo = SimpleClassExpr(List(), Name("foo"))
    val result = ClassLetRecIn(List(Binding(PVar("x"), OInt(7))), foo)
    val expect = """
    let rec x = 7
    in foo
    """
    compareClassExpr(result, expect)
  }

  test("class object") {
    val foo = SimpleClassExpr(List(), Name("foo"))
    val result = ClassObject(ClassBody(List(
      Inherit(foo, Some("bar")),
      Val("a", OInt(7), true, Some(int)),
      Val("b", OInt(7)),
      Val("c", OInt(7),false, Some(int)),
      Method("d", List(PVar("a")), OInt(2), Some(int), true),
      Method("e", List(), OInt(2), None , true),
      Method("f", List(PVar("a")), OInt(2)),
      PolyMethod("p", PolymorphType(List("a", "b"), TypeConstr(ExtendedName("test"))), OInt(2), true),
      Method("p2", List(), OInt(2), Some(int)),
      VirtualMethod("v", int, true),
      VirtualMethod("v2", int)
    ),
      Some(PVar("x")), Some(int)
    ))
    val expect = """
    object
      (x : int)
      inherit foo as bar
      val mutable a : int = 7
      val b = 7
      val c : int = 7
      method private d a : int = 2
      method private e = 2
      method f a = 2
      method private p : 'a 'b . test = 2
      method p2 : int = 2
      method private virtual v : int
      method virtual v2 : int
    end
    """
    compareClassExpr(result, expect)
  }
 /*

  /**
    * Module types
    */

  test("functor type") {
    val result = FunctorType("foo", mt, mt)
    val expect = """
    functor (foo : mt) -> mt
    """
    compareExpr(result, expect)
  }

  test("module type with") {
    val result = MTWith(mt,
      MTTypeConstraint(List(TypeParameter("a", Some(Covariant))), ExtendedName("foo"), int),
      MTModuleConstraint(Name("bar"), ExtendedModulePath(ExtendedModuleName("test")))
    )
    val expect = """
    mt with type +'a foo = int and
    module bar = test
    """
    compareExpr(result, expect)
  }

  test("module sig") {
    val result = Signatur(
      SVal("foo", int),
      SException(ConstrDecl("ex")),
      ModuleSpecification("bar", List(), mt),
      ModuleSpecification("bar2", List(("a",mt), ("b", mt)), mt),
      ModuleTypeSpecification("t"),
      ModuleTypeSpecification("t2", Some(mt)),
      Include(mt),
      Open(Name("tt")),
      ClassSpecification(
        ClassSpec("cl", ClassType(List(), NormalClassBodyType(None)),
          true, List("b", "c")),
        ClassSpec("cl2", ClassType(List(), NormalClassBodyType(None)))
      ),
      ClassTypeDefinition(
        ClassTypeDef("cl",  NormalClassBodyType(None),
          true, List("b", "c")),
        ClassTypeDef("cl2",  NormalClassBodyType(None))
      )
      
    )
    val expect = """
    sig
      val foo : int;;
      exception ex;;
      module bar : mt;;
      module bar2(a : mt) (b : mt) : mt;;
      module type t;;
      module type t2 = mt;;
      include mt;;
      open tt;;
      class virtual ['b 'c] cl : object end 
        and cl2 : object end;;
      class type virtual ['b 'c] cl : object end 
        and cl2 : object end
    end
    """
    compareExpr(result, expect)
  }

  /**
    * Module expression
    */

  test("let and") {
    val result = Let(
      Binding(PVar("x"), OInt(7)),
      Binding(PVar("r"), OInt(7)))
    val expect = """
    let x = 7 
    and r = 7
    """
    compareDef(result, expect)
  }
  test("external") {
    val result = External("input", int, "input")
    val expect = """
    external input : int = "input"
    """
    compareDef(result, expect)
  }
  test("functor") {
    val result = FunctorApp(Functor("f", mt, MEAscription(MVar(Name("x")), mt)), MVar(Name("y")))
    val expect = """
    functor (f : mt) -> (x : mt) (y)
    """
    compareExpr(result, expect)
  }

  test("struct") {
    val result = Struct(
      ModuleTypeDef("foo", mt),
      ModuleDefinition("bar", MVar(Name("n")), List("a" -> mt, "b" -> mt), Some(mt)),
      ModuleDefinition("bar", MVar(Name("n")), List("a" -> mt)),
      ModuleDefinition("bar", MVar(Name("n")) ),
      ModuleDefinition("bar", MVar(Name("n")), List(), Some(mt)),
      OInt(7),
      ClassDefinition(ClassBinding("c", List(), SimpleClassExpr(List(), Name("cc")),
        true, List("a"), Some(ClassType(List(), SimpleClassBodyType(List(), ExtendedName("ct")))))),
      ClassDefinition(ClassBinding("c", List(), SimpleClassExpr(List(), Name("cc")),
        false, List(), None))
    )
    val expect = """
    struct
      module type foo = mt;;
      module bar(a : mt) (b : mt) : mt = n;;
      module bar(a : mt) = n;;
      module bar = n;;
      module bar : mt = n;;
      7;;
      class virtual ['a] c : ct = cc;;
      class c = cc
    end
    """
    compareExpr(result, expect)
  }

  /**
    * Names
    */

  test("extended name") {
    val result = ExtendedName("name", List(
      ExtendedModulePath(ExtendedModuleName("a", List(
        ExtendedModulePath(ExtendedModuleName("aa", List(
          ExtendedModulePath(ExtendedModuleName("aaa")))
        )),
        ExtendedModulePath(ExtendedModuleName("b"))
       )
      )),
      ExtendedModulePath(ExtendedModuleName("c", List(
        ExtendedModulePath(ExtendedModuleName("f"), List(ExtendedModuleName("d")))
      )))
    ))
    val expect = """
    a (aa (aaa)) (b). c (d.f). name
    """
    compareExpr(result, expect)
  }
  */
}
