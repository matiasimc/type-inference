/**
  * Created by mmeneses on 07-09-17.
  */

import org.scalatest._

class InferenceApplyTest extends FlatSpec with Matchers {
  val fun = Fun(Id('foo),TNum(),Id('x),Fun(Id('bar),TNum(),Id('y),Mul(Id('x),Id('y))))
  val expr = Apply(Apply(fun, Num(5)), Num(2))
  val type_and_constraints = TypeChecker.check_type(expr)
  val constraints_unified = TypeChecker.unify(type_and_constraints.cs)
  val type_resolved = TypeChecker.queryType(type_and_constraints.t, constraints_unified)
  println(type_and_constraints)
  println(constraints_unified)
  //val type_resolved = TypeChecker.resolveType(constraints_unified, type_and_constraints.t)
  //println(type_resolved)
  s"The type of the expression $expr" should "return TNum()" in {
    assert(type_resolved == TNum())
  }
}