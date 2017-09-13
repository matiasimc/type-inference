/**
  * Created by mmeneses on 29-08-17.
  */

object TypeChecker {

  var freshIndex = 0

  def getFreshVar : TVar = {
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
        val tv = getFreshVar
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
        val tv = getFreshVar
        val cs = ConstraintSet.union(trid.cs, trarg.cs, ConstraintSet(Constraint(trid.t, TFun(trarg.t, tv))))
        TypeRelation(tv, cs)
      }
    }
  }

  /*
  Substituyo todas las ocurrencias de t1 por t2
   */
  private def substitute(t1: Type, t2: Type, cs: ConstraintSet) : ConstraintSet = {
    cs match {
      case aConstraintSet(c, cs1) => c match {
        case Constraint(tc1, tc2) =>
          if (tc1 == t1) ConstraintSet.union(ConstraintSet(Constraint(t2, tc2)), substitute(t1, t2, cs1))
          else if (tc2 == t1) ConstraintSet.union(ConstraintSet(Constraint(tc1, t2)), substitute(t1, t2, cs1))
          else ConstraintSet.union(ConstraintSet(c), substitute(t1, t2, cs1))
      }
      case EmptyConstraintSet() => EmptyConstraintSet()
    }
  }

  def queryType(tv: Type, subst: Substitution, found : Type = TError()) : Type = {
    subst match {
      case ComplexSubstitution(s1, s2) =>
        queryType(tv, s1, queryType(tv, s2))
      case EmptySubstitution() => found
      case SimpleSubstitution(t1, t2) =>
        if (tv == t1) t2
        else found
    }
  }

  def substituteInConstraintSet(t1: Type, t2: Type, cs: ConstraintSet) : ConstraintSet = {
    cs match {
      case aConstraintSet(c, cs1) => c match {
        case Constraint(t3, t4) => (t3, t4) match {
          case (TFun(ta1, tb1), TFun(ta2, tb2)) =>
            if (t1 == ta1) ConstraintSet.union(ConstraintSet(Constraint(TFun(t2, tb1), t4)), substituteInConstraintSet(t1, t2, cs1))
            else if (t1 == tb1) ConstraintSet.union(ConstraintSet(Constraint(TFun(ta1, t2), t4)), substituteInConstraintSet(t1, t2, cs1))
            else if (t1 == ta2) ConstraintSet.union(ConstraintSet(Constraint(t3, TFun(t2, tb2))), substituteInConstraintSet(t1, t2, cs1))
            else if (t1 == tb2) ConstraintSet.union(ConstraintSet(Constraint(t3, TFun(ta2, t2))), substituteInConstraintSet(t1, t2, cs1))
            else if (t3 == t1) ConstraintSet.union(ConstraintSet(Constraint(t2, t4)), substituteInConstraintSet(t1,t2,cs1))
            else ConstraintSet.union(ConstraintSet(c), substituteInConstraintSet(t1,t2,cs1))
          case (TFun(ta, tb), _) =>
            if (t1 == ta) ConstraintSet.union(ConstraintSet(Constraint(TFun(t2, tb), t4)), substituteInConstraintSet(t1, t2, cs1))
            else if (t1 == tb) ConstraintSet.union(ConstraintSet(Constraint(TFun(ta, t2), t4)), substituteInConstraintSet(t1, t2, cs1))
            else if (t4 == t1) ConstraintSet.union(ConstraintSet(Constraint(t3, t2)), substituteInConstraintSet(t1,t2,cs1))
            else ConstraintSet.union(ConstraintSet(c), substituteInConstraintSet(t1,t2,cs1))
          case (_, TFun(ta, tb)) =>
            if (t1 == ta) ConstraintSet.union(ConstraintSet(Constraint(t3, TFun(t2, tb))), substituteInConstraintSet(t1, t2, cs1))
            else if (t1 == tb) ConstraintSet.union(ConstraintSet(Constraint(t3, TFun(ta, t2))), substituteInConstraintSet(t1, t2, cs1))
            else if (t3 == t1) ConstraintSet.union(ConstraintSet(Constraint(t2, t4)), substituteInConstraintSet(t1,t2,cs1))
            else ConstraintSet.union(ConstraintSet(c), substituteInConstraintSet(t1,t2,cs1))
          case (_,_) =>
            if (t3 == t1) ConstraintSet.union(ConstraintSet(Constraint(t2, t4)), substituteInConstraintSet(t1,t2,cs1))
            else if (t4 == t1) ConstraintSet.union(ConstraintSet(Constraint(t3, t2)), substituteInConstraintSet(t1,t2,cs1))
            else ConstraintSet.union(ConstraintSet(c), substituteInConstraintSet(t1,t2,cs1))
        }
      }
      case EmptyConstraintSet() => EmptyConstraintSet()
    }
  }

  def unify(cs: ConstraintSet) : Substitution = {
    cs match {
      case aConstraintSet(c, cs1) =>
        c match {
          case Constraint(t1, t2) => t1 match {
            case TVar(i) => t2 match {
              case _ => ComplexSubstitution(unify(substituteInConstraintSet(t1,t2,cs1)), SimpleSubstitution(t1, t2))
            }
            case TFun(t3, t4) => t2 match {
              case TFun(t5, t6) => unify(ConstraintSet.union(cs1, ConstraintSet(Constraint(t3, t5), Constraint(t4, t6))))
              case TVar(i) => ComplexSubstitution(unify(substituteInConstraintSet(t2,t1,cs1)), SimpleSubstitution(t2, t1))
            }
            case _ => t2 match {
              case TVar(i) => ComplexSubstitution(unify(substituteInConstraintSet(t2,t1,cs1)), SimpleSubstitution(t2, t1))
              case _ => if (t1 == t2) unify(substituteInConstraintSet(t1,t2,cs1)) else {println("Error in unify 2");sys.exit()}
            }
          }
        }
      case EmptyConstraintSet() => EmptySubstitution()
    }
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