/**
  * Created by mmeneses on 15-08-17.
  */

object Main {
  def main(args: Array[String]): Unit = {
    val exp1 = With(
      Id('x),
      TFun(TNum(), TNum()),
      Fun(
        Id('foo),
        TNum(),
        Id('n),
        TNum(),
        With(
          Id('y),
          TNum(),
          Num(5),
          TNum(),
          IfExpr(
            LesserThan(Id('n), Id('y)),
            Num(0),
            Add(Id('y), Id('n))
          )
        )
      ),
      TNum(),
      Apply(
        Id('x),
        Num(4)
      )
    )
    val exp2 = IfExpr(
      LesserThan(Num(4), Bool(true)),
      Num(0),
      Add(Num(4), Num(5))
    )
    val exp3 = IfExpr(
      LesserThan(Num(4), Num(5)),
      Bool(true),
      Add(Num(4), Num(5))
    )
    val exp4 = IfExpr(
      LesserThan(Num(4), Num(5)),
      Mul(Num(4), Num(5)),
      Add(Num(4), Num(5))
    )
    val exp5 = Fun(
      Id('cuadrado),
      TNum(),
      Id('x),
      TNum(),
      Mul(Id('x), Id('x))
    )
    val res1 = Parser.execute(exp1)
    println(res1)
    println("=====")
    val res1t = Parser.check_type(exp1)
    println(res1t)
    println("=====")
    val res2 = Parser.check_type(exp2)
    println(res2)
    println("=====")
    val res3 = Parser.check_type(exp3)
    println(res3)
    println("=====")
    val res4 = Parser.check_type(exp4)
    println(res4)
    println("=====")
    val res5 = Parser.check_type(exp5)
    println(res5)
    println("====")
  }

}

