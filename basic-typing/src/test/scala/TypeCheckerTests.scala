/**
  * Created by mmeneses on 29-08-17.
  */

import org.scalatest._

class NumTypeCheckTest extends FlatSpec with Matchers {
  val expr1 = Mul(Num(4),Num(5)) // TNum
  val expr2 = Apply(Fun(Id('cuadrado),TNum(),Id('n),TNum(),Mul(Id('n),Id('n))), Num(5)) // TNum
  s"The type of the expression $expr1" should "return TNum" in {
    assert(TypeChecker.check_type(expr1) == TNum())
  }
  s"The type of the expression $expr2" should "return TNum" in {
    assert(TypeChecker.check_type(expr2) == TNum())
  }
}

class BoolTypeCheckTest extends FlatSpec with Matchers {
  val expr1 = Equal(Num(4), Num(5)) // TBool
  val expr2 = Add(Num(5), Num(200)) // TNum
  s"The type of the expression $expr1" should "return TBool" in {
    assert(TypeChecker.check_type(expr1) == TBool())
  }
  s"The type of the expression $expr2" should "not return TBool" in {
    assert(TypeChecker.check_type(expr2) != TBool())
  }
}

class FunTypeCheckTest extends FlatSpec with Matchers {
  val expr1 = Apply(Fun(Id('foo),TNum(),Id('x),TFun(TNum(), TNum()),Fun(Id('bar),TNum(),Id('y),TNum(),Mul(Id('x),Id('y)))), Num(5))
  s"The type of the expression $expr1" should "return TFun(TNum, TNum)" in {
    assert(TypeChecker.check_type(expr1) == TFun(TNum(), TNum()))
  }
}
