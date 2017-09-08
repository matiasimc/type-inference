/**
  * Created by mmeneses on 29-08-17.
  */

object TypeChecker {

  var freshIndex = 0

  def getFreshVar() : TVar = {
    freshIndex += 1
    TVar(freshIndex-1)
  }


  def check_type(expr: Expr, env: Env = EmptyEnv()) : TypeRelation = {
    expr match {
      case Num(n) => TypeRelation(TNum(), EmptyConstraintSet())
      case Bool(b) => TypeRelation(TBool(), EmptyConstraintSet())
      case Id(x) => TypeRelation(env_lookup(env, x).asInstanceOf[Type], EmptyConstraintSet())
      case IfExpr(condPart, thenPart, elsePart) => {
        val c = check_type(condPart, env)
        val t = check_type(thenPart, env)
        val e = check_type(elsePart, env)
        TypeRelation(t.t, ConstraintSet.union(ConstraintSet(Constraint(t.t, e.t), Constraint(c.t, TBool())), t.cs))
      }
      case LesserThan(n1, n2) => check_Bool_type(n1, n2, env)
      case GreaterThan(n1, n2) => check_Bool_type(n1, n2, env)
      case Equal(n1, n2) => check_Bool_type(n1, n2, env)
      case Add(n1, n2) => check_AE_type(n1, n2, env)
      case Subst(n1, n2) => check_AE_type(n1, n2, env)
      case Mul(n1, n2) => check_AE_type(n1, n2, env)
      case Fun(id, tpar, parameter, body) => {
        val trb = check_type(body, aEnv(parameter.s, tpar, env))
        val tv = getFreshVar()
        val cs = aConstraintSet(Constraint(tv, trb.t), trb.cs)
        TypeRelation(TFun(tpar, tv), cs)
      }
      case With(id, tv, value, body) => {
        val trb = check_type(body, aEnv(id.s, tv, env))
        trb
      }
      case Apply(id, arg) => {
        val trid = check_type(id, env)
        val trarg = check_type(arg, env)
        val tv = getFreshVar()
        val cs = ConstraintSet.union(trid.cs, trarg.cs, ConstraintSet(Constraint(trid.t, TFun(trarg.t, tv))))
        TypeRelation(tv, cs)
      }
    }
  }

  /*
  Reemplazo todos los t1 por t2
   */
  private def subst(t1: Type, t2: Type, cs: ConstraintSet) : ConstraintSet = {
    cs match {
      case EmptyConstraintSet() => EmptyConstraintSet()
      case aConstraintSet(c, cs1) => if (t1 == c.t1) aConstraintSet(Constraint(t2, c.t2), subst(t1, t2, cs1))
      else if (t1 == c.t2) aConstraintSet(Constraint(c.t1, t2), subst(t1, t2, cs1))
      else aConstraintSet(c, subst(t1, t2, cs1))
    }
  }

  /*
  Reemplazo las ocurrencias de t1 por t2, en el tipo t
   */
  private def substType(t1: Type, t2: Type, t: Type) : Type = {
    t match {
      case TFun(targ, tbody) => {
        if (targ == t1) TFun(t2, substType(t1, t2, tbody))
        else if (tbody == t1) TFun(substType(t1, t2, targ), t2)
        else TFun(substType(t1, t2, targ), substType(t1, t2, tbody))
      }
      case TVar(i) => {
        if (t1 == t) t2
        else t
      }
      case _ => t
    }
  }

  /*
  Reemplazo las type variable resueltas en un constraint set en un type
   */
  def resolveType(cs : ConstraintSet, t: Type) : Type = {
    cs match {
      case aConstraintSet(c, cs1) =>
        c.t1 match {
          case TVar(i) => resolveType(cs1, substType(c.t1, c.t2, t))
          case _ => resolveType(cs1, t)
        }
      case EmptyConstraintSet() => t
    }
  }

  def unify(cs: ConstraintSet) : ConstraintSet = {
    cs match {
      case aConstraintSet(c, cs1) =>
        c match {
          case Constraint(t1, t2) => t1 match {
            case TVar(i) => aConstraintSet(Constraint(t1, t2), unify(subst(t1, t2, cs1)))
            case TFun(t3, t4) => t2 match {
              case TFun(t5, t6) => unify(ConstraintSet.union(cs1, ConstraintSet(Constraint(t3, t5), Constraint(t4, t6))))
              case _ => println("Error in unify 1"); sys.exit()
            }
            case _ => t2 match {
              case TVar(i) => aConstraintSet(Constraint(t2, t1), unify(subst(t2, t1, cs1)))
              case _ => if (t1 == t2) aConstraintSet(c, unify(cs1)) else {println("Error in unify 2");sys.exit()}
            }
          }
        }
      case EmptyConstraintSet() => cs
    }
  }

  /*
  TODO La unificacion no funciona adecuadamente, por lo que se hizo un "parche" haciendo que el type of haga varias pasadas por las constraints hasta que se resuelva
   */

  def type_of(expr: Expr) : Type = {
    val tr = check_type(expr)
    resolveType(unify(unify(unify(unify(unify(tr.cs))))), tr.t)
  }


  def check_Bool_type(n1: Expr, n2: Expr, env: Env) : TypeRelation = {
      val tr1 = check_type(n1, env)
      val tr2 = check_type(n2, env)
      val cs1 = tr1.cs
      val cs2 = tr2.cs
      TypeRelation(TBool(), ConstraintSet.union(ConstraintSet(Constraint(tr1.t, TNum()), Constraint(tr2.t, TNum())), cs1, cs2))
  }

  def check_AE_type(n1: Expr, n2: Expr, env: Env) : TypeRelation = {
    val tr1 = check_type(n1, env)
    val tr2 = check_type(n2, env)
    val cs1 = tr1.cs
    val cs2 = tr2.cs
    TypeRelation(TNum(), ConstraintSet.union(ConstraintSet(Constraint(tr1.t, TNum()), Constraint(tr2.t, TNum())), cs1, cs2))
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