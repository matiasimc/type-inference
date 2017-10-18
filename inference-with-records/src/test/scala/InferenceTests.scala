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

class InferenceRecordTest extends FlatSpec with Matchers {
  val exp1 = Record(List(RecPair('a, Num(4)), RecPair('b, Fun(Id('foo), TNum(), Id('x), Mul(Id('x), Num(2))))))
  s"The type of the expression $exp1" should "return TRecord(List(TRecPair('a, TNum()), TRecPair('b, TFun(TNum(), TNum()))))" in {
    assert(TypeChecker.typeof(exp1) == TRecord(List(TRecPair('a, TNum()), TRecPair('b, TFun(TNum(), TNum())))))
  }

  val exp2 = Fun(Id('bar), TNum(), Id('x), Record(List(RecPair('a, Id('x)), RecPair('b, Fun(Id('foo), TNum(), Id('y), Mul(Id('x), Id('y)))))))
  s"The type of the expression $exp2" should "return TFun(TNum(), TRecord(List(TRecPair('a, TNum()), TRecPair('b, TFun(TNum(), TNum())))))" in {
    assert(TypeChecker.typeof(exp2) == TFun(TNum(), TRecord(List(TRecPair('a, TNum()), TRecPair('b, TFun(TNum(), TNum()))))))
  }

  val exp3 = Apply(Fun(Id('foo), TRecord(List(TRecPair('a, TNum()))), Id('x), Mul(GetFromRecord(Id('x), 'a), Num(2))), Record(List(RecPair('a, Num(5)))))
  s"The type of the expression $exp3" should "return TNum()" in {
    assert(TypeChecker.typeof(exp3) == TNum())
  }

  val exp4 = GetFromRecord(Record(List(RecPair('a, GetFromRecord(Record(List(RecPair('b, Num(4)))), 'b)))), 'a)
  s"The type of the expression $exp4" should "return TNum()" in {
    assert(TypeChecker.typeof(exp4) == TNum())
  }
}