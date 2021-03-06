/**
  * Created by mmeneses on 27-09-17.
  */

import org.scalatest._

class RecordTest extends FlatSpec with Matchers {
  val expr1 = RecordAccess(Record(List(RecPair('bar, Num(4)))), 'bar)
  s"The evaluation of the expression $expr1" should "return ValInt(4)" in {
    assert(Runner.execute(expr1) == ValInt(4))
  }
  val expr2 = Record(List(RecPair('foo, Fun(TypeChecker.getFreshVar,Id('x),Add(Num(4), Id('x)))),
    RecPair('bar,  Num(4))))
  s"The evaluation of the expression $expr2" should "return ValRecord(List(RecValPair('foo,ValFun('x,Add(Num(4),Id('x)),EmptyEnv())), RecValPair('bar,ValInt(4))))" in {
    assert(Runner.execute(expr2) == ValRecord(List(RecValPair('foo,ValFun('x,Add(Num(4),Id('x)),EmptyEnv())), RecValPair('bar,ValInt(4)))))
  }
  val expr3 = With(Id('r), TypeChecker.getFreshVar, Record(List(RecPair('foo, Num(4)))), Mul(RecordAccess(Id('r), 'foo), Num(3)))
  s"The evaluation of the expression $expr3" should "return ValInt(12)" in {
    assert(Runner.execute(expr3) == ValInt(12))
  }
}