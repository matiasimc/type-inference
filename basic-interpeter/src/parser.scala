import scala.Int

/**
  * Created by mmeneses on 15-08-17.
  */

object Parser {
  def parseExpr(code: String): Expr = {
    return null
  }

  def execute (expr: Expr, env: Env = EmptyEnv()) : ValL = {
    expr match {
      case Num(n) => ValInt(n)
      case Bool(b) => ValBool(b)
      case Add(n1, n2) => makeVal(openInt(execute(n1, env)) + openInt(execute(n2,env)))
      case Subst(n1, n2) => makeVal(openInt(execute(n1, env)) - openInt(execute(n2,env)))
      case Mul(n1, n2) => makeVal(openInt(execute(n1, env)) * openInt(execute(n2,env)))
      case IfExpr(condPart, thenPart, elsePart) => if (openBool(execute(condPart,env))) {
        execute(thenPart,env)
      } else {
        execute(elsePart,env)
      }
      case GreaterThan(n1, n2) => makeVal(openInt(execute(n1, env)) > openInt(execute(n2,env)))
      case LesserThan(n1, n2) => makeVal(openInt(execute(n1, env)) < openInt(execute(n2,env)))
      case Equal(n1, n2) => makeVal(openInt(execute(n1, env)) == openInt(execute(n2,env)))
      //case Fun(id, tp, param, tb, body) => (x : ValL) => execute(body, aEnv(param.s, x, env))
      //case Apply(id, arg) => execute(id, env).asInstanceOf[Any => ValL](execute(arg, env))
      case Fun(id, tp, param, tb, body) => ValFun(param.s, body, env)
      case Apply(e, arg) => {
        execute(e, env) match {
          case ValFun(par, body, env1) => execute(body, aEnv(par, execute(arg,env), env1))
          case _ => {println("Error, expected ValFun"); sys.exit()}
        }
      }
      case With(id, tv, value, tb, b) => execute(b, aEnv(id.s, execute(value, env), env))
      case Id(s) => makeVal(env_lookup(env, s))
      case _ => makeVal(expr)
    }
  }

  def makeVal(sv: Any) : ValL = {
    sv match {
      case n: Int => ValInt(n)
      case b: Boolean => ValBool(b)
      case v: ValL => v
    }
  }

  def openInt(v: ValL) : Int = {
     v match {
      case ValInt(n) => n
      case _ => {println("Compile error"); sys.exit()}
    }
  }

  def openBool(v: ValL) : Boolean = {
    v match {
      case ValBool(b) => b
      case _ => {println("Compile error"); sys.exit()}
    }
  }

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
      case Apply(id, arg) => check_type(id, env) match {
        case TFun(tpar, tbody) => {
          val targr = check_type(arg, env)
          if (targr == tbody) tbody
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

  //def executeFun(expr: Expr): Env = {
  //  case Fun(id, parameter, body) => aEnv(id, body, env)
  //}

  //def replace(arg: Expr, id)
}