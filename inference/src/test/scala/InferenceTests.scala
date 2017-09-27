/**
  * Created by mmeneses on 07-09-17.
  */

import org.scalatest._

class InferenceApplyTest extends FlatSpec with Matchers {
  val fun = Fun(Id('foo),TNum(),Id('x),Fun(Id('bar),TNum(),Id('y),Mul(Id('x),Id('y))))
  val expr = Apply(Apply(fun, Num(5)), Num(2))
  s"The type of the expression $expr" should "return TNum()" in {
    assert(TypeChecker.typeof(expr) == TNum())
  }
}

class InferenceFunBasicTest extends FlatSpec with Matchers {
  val exp1 = Fun(Id('foo), TNum(), Id('x), Mul(Num(4), Id('x)))
  s"The type of the expression $exp1" should "return TFun(TNum(), TNum())" in {
    assert(TypeChecker.typeof(exp1) == TFun(TNum(), TNum()))
  }
}

class InferenceFunComplexTest extends FlatSpec with Matchers {
  val exp2 = Fun(Id('foo), TNum(), Id('x), Fun(Id('bar), TNum(), Id('y), Mul(Id('x), Id('y))))
  s"The type of the expression $exp2" should "return TFun(TNum(), TFun(TNum(), TNum()))" in {
    assert(TypeChecker.typeof(exp2) == TFun(TNum(), TFun(TNum(), TNum())))
  }
}