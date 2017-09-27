/**
  * Created by mmeneses on 29-08-17.
  */

object TypeChecker {
  def check_type(expr: Expr, env: Env = EmptyEnv()) : Type = {
    expr match {
      case Num(n) => TNum()
      case Bool(b) => TBool()
      case Id(x) => env_lookup(env, x).asInstanceOf[Type]
      case IfExpr(condPart, thenPart, elsePart) => {
        val c = check_type(condPart, env)
        val t = check_type(thenPart, env)
        val e = check_type(elsePart, env)
        c match {
          case TBool() => if (t != e) {
            println(s"Type error, $t is not equal to $e")
            TError()
            }
            else {
              t
            }
          case TError() => TError()
          case  _ => {
            println(s"If expression expected type boolean, found $c")
            TError()
          }
        }
      }
      case LesserThan(n1, n2) => check_Bool_type(n1, n2, env)
      case GreaterThan(n1, n2) => check_Bool_type(n1, n2, env)
      case Equal(n1, n2) => check_Bool_type(n1, n2, env)
      case Add(n1, n2) => check_AE_type(n1, n2, env)
      case Subst(n1, n2) => check_AE_type(n1, n2, env)
      case Mul(n1, n2) => check_AE_type(n1, n2, env)
      case Fun(id, tpar, parameter, tbody, body) => {
        val tbr = check_type(body, aEnv(parameter.s, tpar, env))
        if (tbr == tbody) TFun(tpar, tbody)
        else {println(s"Type error, expected $tbody, found $tbr"); TError()}
      }
      case With(id, tv, value, tb, body) => {
        val tvr = check_type(value, env)
        val tbr = check_type(body, aEnv(id.s, tv, env))
        if (tv != tvr) {println(s"Expected a expression with type $tv, not $tvr"); TError()}
        else if (tb != tbr) {println(s"Expected a expression with type $tb, not $tbr"); TError()}
        else tb
      }
      case Record(recPairs) => TRecord(recPairs.map((rp : RecPair) => rp.t))
      case GetFromRecord(r, s) =>
        val rp = r.f.filter((rp : RecPair) => rp.s == s).head
        return check_type(rp.e, env)
      case Apply(id, arg) => check_type(id, env) match {
        case TFun(tpar, tbody) => {
          val targr = check_type(arg, env)
          if (targr == tpar) tbody
          else {println(s"Expected an argument with type $tpar, found $targr");TError()}
        }
        case _ => {println(s"Expected function, found $expr"); TError()}
      }
    }
  }

  def check_Bool_type(n1: Expr, n2: Expr, env: Env) : Type = {
    val t1 = check_type(n1, env)
    val t2 = check_type(n2, env)
    if (t1 == t2) {
      if (t1 == TNum()) TBool() else {println(s"Expression expected type int, found $t1");TError()}
    }
    else {println(s"Type error, $t1 is not equal to $t2"); TError()}
  }

  def check_AE_type(n1: Expr, n2: Expr, env: Env) : Type = {
    val t1 = check_type(n1, env)
    val t2 = check_type(n2, env)
    if (t1 == t2) {
      if (t1 == TNum()) t1 else {println(s"Expression expected type int, found $t1");TError()}
    }
    else {println(s"Type error, $t1 is not equal to $t2"); TError()}
  }

  def env_lookup (env : Env, s : Symbol) : Any = {
    env match {
      case aEnv(sy, v, e) => {
        if (s == sy) v
        else env_lookup(e, s)
      }
      case EmptyEnv() => {
        println(s"Variable $s not found")
        sys.exit()
      }
    }
  }
}