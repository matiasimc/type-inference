/**
  * Created by mmeneses on 07-09-17.
  */

import org.scalatest._

class InferenceIdTest extends FlatSpec with Matchers {
  val expr = With(Id('a), Mul(Mul(Id('a), Num(3)), Num(2)), Id('a))
  s"The type of the expression $expr" should "return TNum()" in {
    assert(TypeChecker.typeof(expr) == TNum())
  }
}

class InferenceApplyTest extends FlatSpec with Matchers {
  val fun = Fun(Id('foo),Id('x),Fun(Id('bar),Id('y),Mul(Id('x),Id('y))))
  val expr = Apply(Apply(fun, Num(5)), Num(2))
  s"The type of the expression $expr" should "return TNum()" in {
    assert(TypeChecker.typeof(expr) == TNum())
  }
}

class InferenceFunBasicTest extends FlatSpec with Matchers {
  val exp1 = Fun(Id('foo), Id('x), Mul(Num(4), Id('x)))
  s"The type of the expression $exp1" should "return TFun(TNum(), TNum())" in {
    assert(TypeChecker.typeof(exp1) == TFun(TNum(), TNum()))
  }
}

class InferenceFunComplexTest extends FlatSpec with Matchers {
  val exp2 = Fun(Id('foo), Id('x), Fun(Id('bar), Id('y), Mul(Id('x), Id('y))))
  s"The type of the expression $exp2" should "return TFun(TNum(), TFun(TNum(), TNum()))" in {
    assert(TypeChecker.typeof(exp2) == TFun(TNum(), TFun(TNum(), TNum())))
  }
}

class InferenceRecordTest1 extends FlatSpec with Matchers {
  val exp1 = Record(List(RecPair('a, Num(4)), RecPair('b, Fun(Id('foo), Id('x), Mul(Id('x), Num(2))))))
  s"The type of the expression $exp1" should "return TRecord(List(TRecPair('a, TNum()), TRecPair('b, TFun(TNum(), TNum()))))" in {
    assert(TypeChecker.typeof(exp1) == TRecord(List(TRecPair('a, TNum()), TRecPair('b, TFun(TNum(), TNum())))))
  }
}

class InferenceRecordTest2 extends FlatSpec with Matchers {
  val exp2 = Fun(Id('bar), Id('x), Record(List(RecPair('a, Mul(Id('x), Num(2))), RecPair('b, Fun(Id('foo), Id('y), Mul(Id('x), Id('y)))))))
  s"The type of the expression $exp2" should "return TFun(TNum(), TRecord(List(TRecPair('a, TNum()), TRecPair('b, TFun(TNum(), TNum())))))" in {
    assert(TypeChecker.typeof(exp2) == TFun(TNum(), TRecord(List(TRecPair('a, TNum()), TRecPair('b, TFun(TNum(), TNum()))))))
  }
}

class InferenceRecordTest3 extends FlatSpec with Matchers {
  val exp3 = Apply(Fun(Id('foo), Id('x), Mul(GetFromRecord(Id('x), 'a), Num(2))), Record(List(RecPair('a, Num(5)))))
  s"The type of the expression $exp3" should "return TNum()" in {
    assert(TypeChecker.typeof(exp3) == TNum())
  }
}

class InferenceRecordTest4 extends FlatSpec with Matchers {
  val exp4 = GetFromRecord(Record(List(RecPair('a, GetFromRecord(Record(List(RecPair('b, Num(4)))), 'b)))), 'a)
  s"The type of the expression $exp4" should "return TNum()" in {
    assert(TypeChecker.typeof(exp4) == TNum())
  }
}

class InferenceRecordTest5 extends FlatSpec with Matchers {
  val exp = With(Id('x), Mul(GetFromRecord(Id('r), 'a), Num(2)), With(Id('y), Add(GetFromRecord(Id('r), 'b), Num(2)), Id('r)))
  s"The type of the expression $exp" should "return TRecord(List(TRecPair('a, TNum()), TRecPair('b, TNum())))" in {
    assert(TypeChecker.typeof(exp) == TRecord(List(TRecPair('a, TNum()), TRecPair('b, TNum()))))
  }
}