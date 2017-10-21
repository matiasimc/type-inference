/**
  * Created by mmeneses on 29-08-17.
  */

object Runner {
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
      case Fun(id, param, body) => ValFun(param.s, body, env)
      case Apply(e, arg) => {
        execute(e, env) match {
          case ValFun(par, body, env1) => execute(body, aEnv(par, execute(arg,env), env1))
          case _ => {println("Error, expected ValFun"); sys.exit()}
        }
      }
      case With(id, value, b) => {
        val nexp = Apply(Fun(Id('with), id, b), value)
        execute(nexp, env)
        execute(b, aEnv(id.s, execute(value, env), env))
      }
      case Record(fields) => ValRecord(fields.map((f : RecPair) => RecValPair(f.s, execute(f.e, env))))
      case GetFromRecord(e,s) =>
        execute(e, env) match {
          case ValRecord(l : List[RecValPair]) => l.filter((rvp: RecValPair) => rvp.s == s).head.v
          case _ => {println("Error, expected ValRecord"); sys.exit()}
        }
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