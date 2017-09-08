/**
  * Created by mmeneses on 07-09-17.
  */

import org.scalatest._

class InferenceFunTest extends FlatSpec with Matchers {
  val expr = Fun(Id('cuadrado),TNum(),Id('n),Mul(Id('n),Id('n)))
  s"The type of the expression $expr" should "return TFun(TNum(), TNum())" in {
    assert(TypeChecker.type_of(expr) == TFun(TNum(), TNum()))
  }
}

class ComplexInferenceFunTest extends FlatSpec with Matchers {
  val expr = Fun(Id('foo),TNum(),Id('x),Fun(Id('bar),TNum(),Id('y),Mul(Id('x),Id('y))))
  s"The type of the expression $expr" should "return TFun(TNum(), TFun(TNum(), TNum()))" in {
    assert(TypeChecker.type_of(expr) == TFun(TNum(), TFun(TNum(), TNum())))
  }
}

class InferenceApplyTest extends FlatSpec with Matchers {
  val fun = Fun(Id('foo),TNum(),Id('x),Fun(Id('bar),TNum(),Id('y),Mul(Id('x),Id('y))))
  val expr = Apply(Apply(fun, Num(5)), Num(2))
  val type_and_constraints = TypeChecker.check_type(expr)
  val constraints_unified = TypeChecker.unify(type_and_constraints.cs)
  println(type_and_constraints)
  println(constraints_unified)
  val type_resolved = TypeChecker.resolveType(constraints_unified, type_and_constraints.t)
  println(type_resolved)
  s"The type of the expression $expr" should "return TNum()" in {
    assert(TypeChecker.type_of(expr) == TNum())
  }
}