/**
  * Created by mmeneses on 15-08-17.
  */

trait Type
case class TNum() extends Type
case class TBool() extends Type
case class TFun(targ : Type, tbody : Type) extends Type
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
case class Fun(id: Id, tpar : Type, parameter: Id, tbody : Type, body: Expr) extends Expr
case class Apply(id: Id, arg: Expr) extends Expr
case class With(id: Id, tv : Type, value: Expr,tb : Type, b:  Expr) extends Expr


trait Env
case class EmptyEnv() extends Env
case class aEnv(id: Symbol, value: Any, env: Env) extends Env

trait ValL
case class ValInt(n: Int) extends ValL
case class ValBool(b: Boolean) extends ValL
case class ValFun(par: Symbol, body: Expr, env: Env) extends ValL

// Hacer que el execute retorne runtime value (ValL, ValBool, ValInt)
// Hacer que Apply reciba una expr en vez de id
// Poner en repositorio