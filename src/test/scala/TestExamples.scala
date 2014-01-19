
package scalaocaml.text
import org.scalatest.FunSuite
import scala.io.Source
import scalaocaml._

trait TestExamples extends FunSuite {
  def compare(result: Any, expect: String)

  implicit def intToOCaml(i: Int) = OInt(i)
  implicit def stringToVar(s: String) = Var(Name(s))
  implicit def tupleToRecordField(t: Tuple2[String, Type]) =
    ImmutableRecordField(t._1, t._2)

  test("int") {
    val result = OInt(2)
    val expect = "2"
    compare(result, expect)
  }

  test("negative int") {
    val result = OInt(-2)
    val expect = "-2"
    compare(result, expect)
  }

  test("float") {
    val result = OFloat(2.3)
    val expect = "2.3"
    compare(result, expect)
  }

  test("string") {
    val result = OString("string")
    val expect = """"string""""
    compare(result, expect)
  }

  test("char") {
    val result = OChar('c')
    val expect = """'c'"""
    compare(result, expect)
  }

  test("unit") {
    val result = Unit
    val expect = "()"
    compare(result, expect)
  }

  test("tagname") {
    val result = TagName("Foo")
    val expect = "`Foo"
    compare(result, expect)
  }

  test("begin end constant") {
    val result = EmptyBeginEnd
    val expect = "begin end"
    compare(result, expect)
  }

  test("int operator") {
    val result = InfixOp(OInt(2), "+", OInt(3))
    val expect = "(2 + 3)"
    compare(result, expect)
  }

  test("prefix binary operator") {
    val result = App("(+)", OInt(2), OInt(3))
    val expect = "((+) 2 3)"
    compare(result, expect)
  }

  test("prefix minus ") {
    val result = UnaryOp("-", "x")
    val expect = "(- x)"
    compare(result, expect)
  }

  test("float operator") {
    val result = InfixOp(OFloat(2.2), "+.", OFloat(3.3))
    val expect = "(2.2 +. 3.3)"
    compare(result, expect)
  }

  test("simple let") {
    val result = Let(Binding(PVar("x"), OInt(3)))
    val expect = "let x = 3"
    println(SyntaxPrettyPrinter.pretty(result))
    compare(result, expect)
  }

  test("fun let 1") {
    val result = Let(FunBinding("square", List(PVar("x")), "x"))
    val expect = "let square x = x"
    compare(result, expect)
  }
  test("fun let 2") {
    val result = Let(FunBinding("ratio", List(PVar("x"), PVar("y")),
      InfixOp(
        App("Float.of_int", "x"), "/.",
        App("Float.of_int", "y"))))
    val expect = """let ratio x y = 
                      ((Float.of_int x) /. (Float.of_int y))"""
    compare(result, expect)
  }

  test("application") {
    val result = App("ratio", OInt(4), OInt(7))
    val expect = """(ratio 4 7)"""
    compare(result, expect)
  }

  test("if then else") {
    val result = IfThenElse(True, OInt(1), OInt(0))
    val expect = """if true then 1 else 0"""
    compare(result, expect)
  }

  test("if then") {
    val result = IfThen(True, OInt(1))
    val expect = """if true then 1"""
    compare(result, expect)
  }

  test("tuple") {
    val result = Tuple(OInt(3), OInt(2))
    val expect = """(3, 2)"""
    compare(result, expect)
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
    compare(result, expect)
  }

  test("list") {
    val result = OList(OString("OCaml"), OString("Perl"), OString("C"))
    val expect = """["OCaml"; "Perl"; "C"]"""
    compare(result, expect)
  }

  test("list ::") {
    val result = InfixOp(OInt(1), "::", InfixOp(OInt(2), "::", OList()))
    val expect = """(1 :: (2 :: []))"""
    compare(result, expect)
  }

  test("list @ list") {
    val result = InfixOp(OList(1, 2, 3), "@", OList(4, 5, 6))
    val expect = """([1; 2; 3] @ [4; 5; 6])"""
    compare(result, expect)
  }

  test("let list pattern") {
    val result = Let(FunBinding("my_favorite_language", List(
      PList(PVar("my_favorite"), PVar("the_rest"))), "my_favorite"))
    val expect = """let my_favorite_language (my_favorite :: the_rest) =
     my_favorite"""
    compare(result, expect)
  }

  test("match list pattern") {
    val result = Match("languages",
      Matching(PList(PVar("first"), PVar("the_rest")), "first"),
      Matching(EmptyList, OString("OCaml")))
    val expect = """
    match languages with
    | (first :: the_rest) -> first
    | [] -> "OCaml""""
    compare(result, expect)
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
    compare(result, expect)
  }

  test("let in") {
    val result = LetIn(List(Binding(PVar("x"), OInt(7))), InfixOp("x", "+", "x"))
    val expect = """
    let x = 7 in
        (x + x)
    """
    compare(result, expect)
  }

  test("let and") {
    val result = Let(
      Binding(PVar("x"), OInt(7)),
      Binding(PVar("r"), OInt(7)))
    val expect = """
    let x = 7 
    and r = 7
    """
    compare(result, expect)
  }

  test("nested let rec in") {
    val result = LetIn(List(Binding(PVar("x"), OInt(7))),
      LetIn(List(Binding(PVar("y"), InfixOp("x", "*", "x"))), InfixOp("x", "+", "y")))
    val expect = """
    let x = 7 in
    let y = (x * x) in
    (x + y)
    """
    compare(result, expect)
  }

  test("record type") {
    val result = TypeDecl(List(), "point2d",
      TRecord("x" -> TVar("float"), "y" -> TVar("float")))
    val expect = """
    type point2d = {x : float; y : float}
    """
    compare(result, expect)
  }

  test("record") {
    val result = Record(Map(Name("x") -> OFloat(3.0), Name("y") -> OFloat(-4.0)))
    val expect = """
    {x = 3.0; y = -4.0}
    """
    compare(result, expect)
  }

  test("let record pattern") {
    val result = Let(FunBinding("mag",
      List(PRecord(Map(Name("x") -> PVar("x_pos"), Name("y") -> PVar("y_pos")))),
      "y_pos"))
    val expect = """
    let mag {x = x_pos; y = y_pos} = y_pos
    """
    compare(result, expect)
  }

  test("let record pattern field punning") {
    val result = Let(FunBinding("mag", List(RecordPunning(Name("x"), Name("y"))), "x"))
    val expect = """
    let mag {x; y} = x 
    """
    compare(result, expect)
  }

  test("record access") {
    val result = RecordAccess("r", Name("x"))
    val expect = """r.x"""
    compare(result, expect)
  }

  test("record copy") {
    val result = RecordCopy("r", Map(Name("x") -> OInt(2),
         Name("y") -> OInt(3)))
    val expect = """{r with x = 2; y = 3}"""
    compare(result, expect)
  }

  test("variant type") {
    val result = TypeDecl(List(), "foobar",
      TVariant(VariantField("Foo", TVar("int")), ConstantField("Bar")))
    val expect = """
    type foobar = | Foo of int| Bar  
    """
    compare(result, expect)
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
    compare(result, expect)
  }

  test("array") {
    val result = Array(OInt(1), OInt(2), OInt(3))
    val expect = """
    [|1; 2; 3|]
    """
    compare(result, expect)
  }

  test("array access") {
    val result = ArrayAccess("numbers", OInt(2))
    val expect = """
    numbers.(2)
    """
    compare(result, expect)
  }

  test("array update") {
    val result = ArrayUpdate("numbers", OInt(2), OInt(4))
    val expect = """
    numbers.(2) <- 4
    """
    compare(result, expect)
  }

  test("mutable record type") {
    val result = TypeDecl(List(), "running_sum",
      TRecord(MutableRecordField("sum", TVar("float")),
        MutableRecordField("samples", TVar("int"))))
    val expect = """
    type running_sum =
    {mutable sum : float;
      mutable samples : int}
    """
    compare(result, expect)
  }

  test("unit as pattern") {
    val result = Let(FunBinding("create", List(Unit), Record(Map(Name("sum") -> OFloat(0.0)))))
    val expect = """let create () = {sum = 0.0}
    """
    compare(result, expect)
  }

  test("update mutable record and sequence") {
    val result = Sequence(RecordUpdate("r", Name("samples"), OInt(1)),
      RecordUpdate("r", Name("sum"), "x"))
    val expect = """
     (r.samples <- 1;
     r.sum     <- x)
    """
    compare(result, expect)
  }

  test("unit type") {
    val result = TVar("unit")
    val expect = "unit"
    compare(result, expect)
  }

  test("polymorphic type") {
    val result = TypeDecl(List("a"), "ref", TRecord(MutableRecordField("contents", PolyVar("a"))))
    val expect = """type ('a) ref = {mutable contents : 'a}"""
    compare(result, expect)
  }

  test("let rec") {
    val result = LetRec(FunBinding("f", List(PVar("x")), App("f", "x")))
    val expect = """let rec f x = (f x)"""
    compare(result, expect)
  }

  test("anonymous functions") {
    val result = Fun(List(
      PVar("x"),
      LabeledArg("y"),
      LabeledArgWithPattern("z", EmptyArray),
      OptionalLabeledArg("l", None, Some(TVar("int option")), Some(Constr(Name("Some"), OInt(0)))), //TODO int option is not a TVar
      OptionalLabeledArg("t", Some(OInt(0)), Some(TVar("int")), Some(OInt(0)))),
      "x",
      Some("y"))
    val expect = """
    (fun x ~y ~z : [||] ?(l : int option = Some(0)) ?t : (0 : int = 0) when y  -> x)
    """
    compare(result, expect)
  }

  test("or pattern and underscore pattern") {
    val result = Match("list",
      Matching(OrPattern(FixSizeList(List()), FixSizeList(List(Underscore))), "None"))
    val expect = """match list with
    | [] | [_] -> None"""
    compare(result, expect)
  }

  test("let operator ") {
    val result = Let(FunBinding("(|>)", List(PVar("x"), PVar("f")), App("f", "x")))
    val expect = """ let (|>) x f = (f x)"""
    compare(result, expect)
  }

  test("for to") {
    val result = ForTo("i", OInt(1), OInt(10),
      App("print_int", "i"))
    val expect = """
    for i = 1 to 10 do (print_int i) done"""
    compare(result, expect)
  }

  test("for downto") {
    val result = ForDown("i", OInt(1), OInt(10),
      App("print_int", "i"))
    val expect = """
    for i = 1 downto 10 do (print_int i) done"""
    compare(result, expect)
  }

  test("while") {
    val result = While(True, "x")
    val expect = """
    while true
    do x done
    """
    compare(result, expect)
  }

  test("function with pattern matching and guard") {
    val result = Function(
      MatchingWithGuard(ConstrPattern(Name("Pair"), PVar("x"), PVar("y")), True, "x"),
      Matching(Underscore, OInt(0)))
    val expect = """ function
     | Pair(x, y) when true -> x
     | _ -> 0"""
    compare(result, expect)
  }

  test("ascription ") {
    val result = Ascription(OInt(1), TVar("int"))
    val expect = """ (1 : int)"""
    compare(result, expect)
  }

  test("let labeled arguments") {
    val result = Let(FunBinding("ratio", List(
      LabeledArg("num"),
      LabeledArg("denom")), "num"))
    val expect = """ 
    let ratio ~num ~denom = num 
    """
    compare(result, expect)
  }

  test("try") {
    val result = Try(
      App(App("List.assoc", "digit"), OList(OInt(2))),
      Matching(ConstrPattern(Name("Not_found")), OString("not found")))
    val expect = """ 
    try
      ((List.assoc digit) [2])
    with | Not_found ->
      "not found"
    """
    compare(result, expect)
  }

  test("char of") {
    val result = CharOf(OString("foo"), OInt(2))
    val expect = """ 
    "foo".[2]
    """
    compare(result, expect)
  }

  test("update string") {
    val result = UpdateString(OString("foo"), OInt(2), OChar('c'))
    val expect = """ 
    "foo".[2] <- 'c'
    """
    compare(result, expect)
  }

  test("begin end") {
    val result = BeginEnd(OInt(2)) 
    val expect = """ 
    begin 2 end
    """
    compare(result, expect)
  }

  test("new") {
    val result = New(Name("foo.bar"))
    val expect = """ 
    new foo.bar
    """
    compare(result, expect)
  }

  test("method call") {
    val result = MethodCall("foo", "fun")
    val expect = """ 
    foo # fun
    """
    compare(result, expect)
  }

  test("instance variable") {
    val result = InstVar("foo")
    val expect = """ 
    foo
    """
    compare(result, expect)
  }

  test("assign instance variable") {
    val result = AssignInstVar("foo", OInt(2))
    val expect = """ 
    foo <- 2
    """
    compare(result, expect)
  }

  test("array and alias pattern") {
    val result = Alias(ArrayPattern(OInt(1),OInt(2)), "a")
    val expect = """ 
    [|1; 2|] as a
    """
    compare(result, expect)
  }

  test("Typeconstr pattern") {
    val result = PTypeconstr(Name("foo")) 
    val expect = """ 
    #foo
    """
    compare(result, expect)
  }




  /*
  test("labeled arguments application"){
    val result = OInt(2)
    val expect = """ 
    ratio ~num:3 ~denom:10
    """
    compare(result, expect)
  }

  test("labeled arguments application with punning"){
    val result = OInt(2)
    val expect = """ 
    let num = 3 in
    let denom = 4 in
    ratio ~num ~denom
    """
    compare(result, expect)
  }

  test("let optional arguments"){
    val result = OInt(2)
    val expect = """ 
    let concat ?sep x y = sep
    """
    compare(result, expect)
  }

  test("let optional arguments with default"){
    val result = OInt(2)
    val expect = """ let concat ?(sep="") x y = x ^ sep ^ y 
    """
    compare(result, expect)
  }

  test("match list with literal"){
    val result = OInt(2)
    val expect = """ 
    match l with
    | [] -> 2
    | 0  :: tl -> 5
    """
    compare(result, expect)
  }

  */

}
