/**
  * Created by mmeneses on 27-09-17.
  */

import org.scalatest._

class RecordTest extends FlatSpec with Matchers {
  val expr1 = GetFromRecord(Record(List(RecPair('bar, TNum(), Num(4)))), 'bar)
  s"The evaluation of the expression $expr1" should "return ValInt(4)" in {
    assert(Runner.execute(expr1) == ValInt(4))
  }
  val expr2 = Record(List(RecPair('foo, TFun(TNum(), TNum()), Fun(Id('fun),TNum(),Id('x),TNum(),Add(Num(4), Id('x)))),
                          RecPair('bar, TNum(), Num(4))))
  s"The evaluation of the expression $expr2" should "return ValRecord(List(RecValPair('foo,ValFun('x,Add(Num(4),Id('x)),EmptyEnv())), RecValPair('bar,ValInt(4))))" in {
    assert(Runner.execute(expr2) == ValRecord(List(RecValPair('foo,ValFun('x,Add(Num(4),Id('x)),EmptyEnv())), RecValPair('bar,ValInt(4)))))
  }
}