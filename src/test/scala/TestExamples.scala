
package scalaocaml.text
import org.scalatest.FunSuite
import scala.io.Source
import scalaocaml._

trait TestExamples extends FunSuite {
  def compare(result: Any, expect: String)

  implicit def intToOCaml(i:Int) = OInt(i)
  implicit def stringToVar(s:String) = Var(s) 
  implicit def tupleToRecordField(t: Tuple2[String,Type]) = 
    ImmutableRecordField(t._1,t._2)

  test("int"){
    val result = OInt(2)
    val expect = "2"
    compare(result, expect)
  }

  test("negative int"){
    val result = OInt(-2)
    val expect = "-2"
    compare(result, expect)
  }

  test("float"){
    val result = OFloat(2.3)
    val expect = "2.3"
    compare(result, expect)
  }

  test("string"){
    val result = OString("string") 
    val expect = """"string""""
    compare(result, expect)
  }

  test("int operator"){
    val result = InfixOp(OInt(2),"+", OInt(3))
    val expect = "(2 + 3)"
    compare(result, expect)
  }

  test("prefix binary operator"){
    val result = PrefixBinaryOp("+", OInt(2), OInt(3)) 
    val expect = "((+) 2 3)"
    compare(result, expect)
  }

  test("prefix minus "){
    val result = UnaryOp("-","x") 
    val expect = "(- x)"
    compare(result, expect)
  }

  test("float operator"){
    val result = InfixOp(OFloat(2.2), "+.", OFloat(3.3))
    val expect = "(2.2 +. 3.3)"
    compare(result, expect)
  }

  test("simple let"){
    val result = Let(PVar("x"),OInt(3)) 
    val expect = "let x = 3"
    println(SyntaxPrettyPrinter.pretty(result))
    compare(result, expect)
  }

  test("fun let 1"){
    val result = LetFun("square", List(PVar("x")), Var("x"))
    val expect = "let square x = x"
    compare(result, expect)
  }
  test("fun let 2"){
    val result = LetFun("ratio", List(PVar("x"),PVar("y")),
                     InfixOp(
                        App(Var("Float.of_int"),Var("x"))  , "/.",
                        App(Var("Float.of_int"),Var("y"))  
                     ))
    val expect = """let ratio x y = 
                      ((Float.of_int x) /. (Float.of_int y))"""
    compare(result, expect)
  }

  test("application"){
    val result = App(App(Var("ratio"),OInt(4)),OInt(7)) 
    val expect = """((ratio 4) 7)"""
    compare(result, expect)
  }

  test("if"){
    val result = Conditional(True(), OInt(1), OInt(0)) 
    val expect = """if true then 1 else 0"""
    compare(result, expect)
  }

  test("tuple"){
    val result = Tuple(OInt(3),OInt(2))
    val expect = """(3, 2)"""
    compare(result, expect)
  }

  test("let tuple pattern"){
    val result = LetFun("di", 
      List(PTuple(PVar("x1"),PVar("y1")),
           PTuple(PVar("x2"),PVar("y2"))),
           App(Var("sqrt"),
             InfixOp(
               InfixOp(Var("x1"), "-.", Var("x2"))
               ,"**",OFloat(2.0))))
    val expect = """
    let di (x1, y1) (x2, y2) = 
        (sqrt ((x1 -. x2) ** 2.0))
    """
    compare(result, expect)
  }

  test("list"){
    val result = OList(OString("OCaml"),OString("Perl"),OString("C"))
    val expect = """["OCaml"; "Perl"; "C"]"""
    compare(result, expect)
  }

  test("list ::"){
    val result = InfixOp(OInt(1), "::", InfixOp(OInt(2), "::", OList()))
    val expect = """(1 :: (2 :: []))"""
    compare(result, expect)
  }

  test("list @ list"){
    val result = InfixOp(OList(1,2,3), "@", OList(4,5,6))
    val expect = """([1; 2; 3] @ [4; 5; 6])"""
    compare(result, expect)
  }

  test("let list pattern"){
    val result = LetFun("my_favorite_language",List( 
      PList(PVar("my_favorite"),PVar("the_rest"))), Var("my_favorite"))
    val expect = """let my_favorite_language (my_favorite :: the_rest) =
     my_favorite"""
    compare(result, expect)
  }

  test("match  list pattern"){
    val result = Match(Var("languages"),
       Branch(PList(PVar("first"),PVar("the_rest")), Var("first")),
       Branch(PList(),OString("OCaml")
      ))
    val expect = """
    match languages with
    | (first :: the_rest) -> first
    | [] -> "OCaml"""" 
    compare(result, expect)
  }

  test("options"){
    val result = LetFun("divide", List(PVar("x"),PVar("y")),
      Conditional(InfixOp(Var("y"),"=", OInt(0)),
        Var("None"),
        App("Some", InfixOp("x", "/", "y"))))
    val expect = """
    let divide x y = 
    if (y = 0) then None else (Some (x / y))
    """ 
    compare(result, expect)
  }

  test("let in"){
    val result = LetIn(PVar("x"), OInt(7), InfixOp("x","+", "x"))
    val expect = """
    let x = 7 in
        (x + x)
    """ 
    compare(result, expect)
  }

  test("nested let in"){
    val result = LetIn(PVar("x"), OInt(7), 
      LetIn(PVar("y"), InfixOp("x","*", "x"), InfixOp("x", "+","y")))
    val expect = """
    let x = 7 in
    let y = (x * x) in
    (x + y)
    """ 
    compare(result, expect)
  }

  test("record type"){
    val result = TypeDecl(List(), "point2d", 
      TRecord("x" -> TVar("float"), "y" -> TVar("float")))
    val expect = """
    type point2d = {x : float; y : float}
    """ 
    compare(result, expect)
  }

  test("record"){
    val result = Record(Map("x" -> OFloat(3.0), "y" -> OFloat(-4.0))) 
    val expect = """
    {x = 3.0; y = -4.0}
    """ 
    compare(result, expect)
  }

  test("let record pattern"){
    val result = LetFun("mag", 
      List(PRecord(Map("x"-> PVar("x_pos"), "y" -> PVar("y_pos")))),
      "y_pos")
    val expect = """
    let mag {x = x_pos; y = y_pos} = y_pos
    """ 
    compare(result, expect)
  }

  test("let record pattern field punning"){
    val result = OInt(2)
    val expect = """
    let magnitude { x; y } = sqrt (x ** 2. +. y ** 2.)
    """ 
    compare(result, expect)
  }

  test("record access"){
    val result = RecordAccess("r", "x") 
    val expect = """r.x""" 
    compare(result, expect)
  }

  test("variant type"){
    val result = TypeDecl(List(), "foobar",
      TVariant(VariantField("Foo", TVar("int")), Constant("Bar")))
    val expect = """
    type foobar = | Foo of int| Bar  
    """ 
    compare(result, expect)
  }

  test("match variant"){
    val result = Match("fb", 
      Branch(PVariant("Foo",PVar("x")), "x"),
      Branch(PVariant("Bar",PVar("x")), "x"))
    val expect = """
    match fb with
    | Foo(x) -> x
    | Bar(x) -> x
    """ 
    compare(result, expect)
  }

  test("array"){
    val result = OInt(2)
    val expect = """
    [|1; 2; 3|]
    """ 
    compare(result, expect)
  }

  test("mutable record type"){
    val result = TypeDecl(List(),"running_sum", 
      TRecord(MutableRecordField("sum" , TVar("float")),
        MutableRecordField("samples" , TVar("int"))))
    val expect = """
    type running_sum =
    {mutable sum : float;
      mutable samples : int}
    """ 
    compare(result, expect)
  }

  test("unit as pattern"){
    val result = LetFun("create",List(PUnit),Record(Map("sum" -> OFloat(0.0))))
    val expect = """let create () = {sum = 0.0}
    """ 
    compare(result, expect)
  }

  test("update mutable record and sequence"){
    val result = Sequence(RecordUpdate("r", "samples", OInt(1)),
      RecordUpdate("r", "sum", Var("x")))
    val expect = """
     r.samples <- 1;
     r.sum     <- x
    """ 
    compare(result, expect)
  }

  test("unit"){
    val result = Unit 
    val expect = "()"
    compare(result, expect)
  }

  test("unit type"){
    val result = TVar("unit")
    val expect = "unit"
    compare(result, expect)
  }

  test("polymorphic type"){
    val result = TypeDecl(List("a"), "ref", TRecord(MutableRecordField("contents", PolyVar("a"))))
    val expect = """type ('a) ref = {mutable contents : 'a}"""
    compare(result, expect)
  }

  test("let rec"){
    val result = LetRec("f", List(PVar("x")), App("f","x")) 
    val expect = """let rec f x = (f x)"""
    compare(result, expect)
  }

  test("anonymous functions"){
    val result = Fun(List("x", "y"), "x") 
    val expect = """
    (fun x y -> x)
    """
    compare(result, expect)
  }
  test("or pattern and underscore pattern"){
    val result = Match("list", 
      Branch(OrPattern(FixSizeList(List()),FixSizeList(List(Underscore))),"None"))
    val expect = """match list with
    | [] | [_] -> None"""
    compare(result, expect)
  }

  test("let operator "){
    val result = LetFun("(¦>)", List(PVar("x"), PVar("f")), App("f", "x"))
    val expect = """ let (|>) x f = (f x)"""
    compare(result, expect)
  }

/*
  test("for"){
    val result = OInt(2)
    val expect = """
    let f array =
    let length = Array.length array in
    for i = 0 to length - 1 do
       array.(i) <- array.(i) + 1
    done
    """
    compare(result, expect)
  }

  test("while"){
    val result = OInt(2)
    val expect = """
    let find_first_negative_entry array =
    let pos = ref 0 in
    while
      let pos_is_good = !pos < Array.length array in
      let element_is_non_negative = array.(!pos) >= 0 in
      pos_is_good && element_is_non_negative
    do
      pos := !pos + 1
    done;
    if !pos = Array.length array then None else Some !pos
    """
    compare(result, expect)
  }

  test("function with pattern matchting "){
    val result = OInt(2)
    val expect = """ function
     | Some x -> x
     | None -> 0"""
    compare(result, expect)
  }

  test("let labeled arguments"){
    val result = OInt(2)
    val expect = """ 
    let ratio ~num ~denom = float num /. float denom
    """
    compare(result, expect)
  }

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
