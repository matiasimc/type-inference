
/**
  * Created by mmeneses on 15-08-17.
  */

trait Type
case class TNum() extends Type
case class TBool() extends Type
case class TVar(index: Int) extends Type
case class TFun(targ : Type, tbody : Type) extends Type
case class TRecord(tfields: Set[TField]) extends Type
case class TField(name: Symbol, t: Type)
case class Pi(tfields : Set[FieldType], abs: Absent) extends Type
case class FieldType(name: Symbol, fieldFlag : FlagVariable, t: Type)
case class Absent(nfields : Int) extends Type
case class FlagVariable(index: Int)
case class TError() extends Type

trait Expr
case class Num(n: Int) extends Expr
case class Bool(b: Boolean) extends Expr
case class Add(n1: Expr, n2: Expr) extends Expr
case class Subst(n1: Expr, n2: Expr) extends Expr
case class Mul(n1: Expr, n2: Expr) extends Expr
case class LesserThan(n1: Expr, n2: Expr) extends Expr
case class GreaterThan(n1: Expr, n2: Expr) extends Expr
case class Equal(n1: Expr, n2: Expr) extends Expr
case class Id(s: Symbol) extends Expr
case class IfExpr(condPart: Expr, thenPart: Expr, elsePart: Expr) extends Expr
case class Fun(tpar: Type, parameter: Id, body: Expr) extends Expr
case class Apply(e: Expr, arg: Expr) extends Expr
case class With(id: Id, tv : Type, value: Expr,b:  Expr) extends Expr
case class Record(f: List[RecPair]) extends Expr
case class RecPair(s: Symbol, e: Expr)
case class RecordAccess(e: Expr, s: Symbol) extends Expr


trait Env
case class EmptyEnv() extends Env
case class aEnv(id: Symbol, value: Any, env: Env) extends Env

case class Constraint(t1: Type, t2: Type)

trait ConstraintSet
case class EmptyConstraintSet() extends ConstraintSet
case class aConstraintSet(c: Constraint, cs: ConstraintSet) extends ConstraintSet

object ConstraintSet {
  def apply(c: Constraint*) : ConstraintSet = {
    c.foldRight(EmptyConstraintSet(): ConstraintSet)((cr, cs) => aConstraintSet(cr, cs))
  }

  def union(cs: ConstraintSet*) : ConstraintSet = {
    cs.reduce(
      (cs1, cs2) => cs1 match {
        case EmptyConstraintSet() => cs2
        case aConstraintSet(c, cs3) => aConstraintSet(c, union(cs3, cs2))
      }
    )
  }

}

case class TypeRelation(t: Type, cs: ConstraintSet, env: Env)

trait Substitution

case class EmptySubstitution() extends Substitution
case class SimpleSubstitution(t1: Type, t2: Type) extends Substitution
case class ComplexSubstitution(s1: Substitution, s2: Substitution) extends Substitution

trait ValL
case class ValInt(n: Int) extends ValL
case class ValBool(b: Boolean) extends ValL
case class ValFun(par: Symbol, body: Expr, env: Env) extends ValL
case class RecValPair(s: Symbol, v: ValL)
case class ValRecord(f: List[RecValPair]) extends ValL

// Hacer que el execute retorne runtime value (ValL, ValBool, ValInt)
// Hacer que Apply reciba una expr en vez de id
// Poner en repositorio