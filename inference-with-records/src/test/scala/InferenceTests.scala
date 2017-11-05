/**
  * Created by mmeneses on 07-09-17.
  */

import org.scalatest._

class InferenceIdTest extends FlatSpec with Matchers {
  val expr = With(Id('a), TypeChecker.getFreshVar, Mul(Mul(Id('a), Num(3)), Num(2)), Id('a))
  s"The type of the expression $expr" should "return TNum()" in {
    assert(TypeChecker.typeof(expr) == TNum())
  }
}

class InferenceApplyTest extends FlatSpec with Matchers {
  val fun = Fun(TNum(),Id('x),Fun(TypeChecker.getFreshVar,Id('y),Mul(Id('x),Id('y))))
  val expr = Apply(Apply(fun, Num(5)), Num(2))
  s"The type of the expression $expr" should "return TNum()" in {
    assert(TypeChecker.typeof(expr) == TNum())
  }
}

class InferenceFunBasicTest extends FlatSpec with Matchers {
  val exp1 = Fun(TNum(), Id('x), Mul(Num(4), Id('x)))
  s"The type of the expression $exp1" should "return TFun(TNum(), TNum())" in {
    assert(TypeChecker.typeof(exp1) == TFun(TNum(), TNum()))
  }
}

class InferenceFunComplexTest1 extends FlatSpec with Matchers {
  val exp2 = Fun(TNum(), Id('x), Fun(TypeChecker.getFreshVar, Id('y), Mul(Id('x), Id('y))))
  s"The type of the expression $exp2" should "return TFun(TNum(), TFun(TNum(), TNum()))" in {
    assert(TypeChecker.typeof(exp2) == TFun(TNum(), TFun(TNum(), TNum())))
  }
}

class InferenceFunComplexTest2 extends FlatSpec with Matchers {
  val exp1 = With(Id('a), TypeChecker.getFreshVar, Mul(Num(2), Apply(Id('foo), Bool(true))), Id('foo))
  val exp2 = Fun(TypeChecker.getFreshVar, Id('foo), exp1)
  s"The type of the expression $exp2" should "return TFun(TFun(TBool(), TNum()), TFun(TBool(), TNum()))" in {
    assert(TypeChecker.typeof(exp2) == TFun(TFun(TBool(), TNum()), TFun(TBool(), TNum())))
  }
}

class InferenceRecordTest1 extends FlatSpec with Matchers {
  val exp1 = Record(List(RecPair('a, Num(4)), RecPair('b, Fun(TypeChecker.getFreshVar, Id('x), Mul(Id('x), Num(2))))))
  s"The type of the expression $exp1" should "return TRecord(Set(TField('a, TNum()), TField('b, TFun(TNum(), TNum()))))" in {
    assert(TypeChecker.typeof(exp1) == TRecord(Set(TField('a, TNum()), TField('b, TFun(TNum(), TNum())))))
  }
}

class InferenceRecordTest2 extends FlatSpec with Matchers {
  val exp2 = Fun(TypeChecker.getFreshVar, Id('x), Record(List(RecPair('a, Mul(Id('x), Num(2))), RecPair('b, Fun(TypeChecker.getFreshVar, Id('y), Mul(Id('x), Id('y)))))))
  s"The type of the expression $exp2" should "return TFun(TNum(), TRecord(Set(TField('a, TNum()), TField('b, TFun(TNum(), TNum())))))" in {
    assert(TypeChecker.typeof(exp2) == TFun(TNum(), TRecord(Set(TField('a, TNum()), TField('b, TFun(TNum(), TNum()))))))
  }
}

class InferenceRecordTest3 extends FlatSpec with Matchers {
  val exp3 = Apply(Fun(TypeChecker.getFreshVar, Id('x), Mul(RecordAccess(Id('x), 'a), Num(2))), Record(List(RecPair('a, Num(5)))))
  s"The type of the expression $exp3" should "return TNum()" in {
    assert(TypeChecker.typeof(exp3) == TNum())
  }
}

class InferenceRecordTest4 extends FlatSpec with Matchers {
  val exp4 = RecordAccess(Record(List(RecPair('a, RecordAccess(Record(List(RecPair('b, Num(4)))), 'b)))), 'a)
  s"The type of the expression $exp4" should "return TNum()" in {
    assert(TypeChecker.typeof(exp4) == TNum())
  }
}

class InferenceRecordTest5 extends FlatSpec with Matchers {
  val exp1 = With(Id('x), TypeChecker.getFreshVar, Mul(RecordAccess(Id('r), 'a), Num(2)), With(Id('y),TypeChecker.getFreshVar ,Add(RecordAccess(Id('r), 'b), Num(2)), Id('r)))
  val exp = Fun(TypeChecker.getFreshVar, Id('r), exp1)
  s"The type of the expression $exp" should "return TFun(TRecord(Set(TField('a, TNum()), TField('b, TNum()))), TRecord(Set(TField('a, TNum()), TField('b, TNum()))))" in {
    assert(TypeChecker.typeof(exp) == TFun(TRecord(Set(TField('a, TNum()), TField('b, TNum()))), TRecord(Set(TField('a, TNum()), TField('b, TNum())))))
  }
}