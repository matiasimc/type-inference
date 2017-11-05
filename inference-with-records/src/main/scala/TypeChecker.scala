/**
  * Created by mmeneses on 29-08-17.
  */

object TypeChecker {

  private var freshIndex = 0

  def getFreshVar : TVar = {
    freshIndex += 1
    TVar(freshIndex-1)
  }

  private var flagIndex = 0
  private var flagMap = Map[Symbol, FlagVariable]()

  private def getFlagVar(name: Symbol) : FlagVariable = {
    flagMap.get(name) match {
      case Some(x) => x
      case None =>
        flagIndex += 1
        val flag = FlagVariable(flagIndex-1)
        flagMap = flagMap + (name -> flag)
        flag
    }
  }


  def check_type(expr: Expr, env: Env = EmptyEnv()) : TypeRelation = {
    expr match {
      case Num(n) => TypeRelation(TNum(), EmptyConstraintSet(), env)
      case Bool(b) => TypeRelation(TBool(), EmptyConstraintSet(), env)
      case Id(x) => {
        val t = env_lookup(env, x).asInstanceOf[Type]
        TypeRelation(t, EmptyConstraintSet(), aEnv(x, t, env))
      }
      case IfExpr(condPart, thenPart, elsePart) => {
        val c = check_type(condPart, env)
        val t = check_type(thenPart, env)
        val e = check_type(elsePart, env)
        TypeRelation(t.t, ConstraintSet.union(ConstraintSet(Constraint(t.t, e.t), Constraint(c.t, TBool())), t.cs), env)
      }
      case LesserThan(n1, n2) => check_Bool_type(n1, n2, env)
      case GreaterThan(n1, n2) => check_Bool_type(n1, n2, env)
      case Equal(n1, n2) => check_Bool_type(n1, n2, env)
      case Add(n1, n2) => check_AE_type(n1, n2, env)
      case Subst(n1, n2) => check_AE_type(n1, n2, env)
      case Mul(n1, n2) => check_AE_type(n1, n2, env)
      case Fun(tpar, parameter, body) => {
        val trb = check_type(body, aEnv(parameter.s, tpar, env))
        val tv1 = getFreshVar
        val tv2 = getFreshVar
        val cs = ConstraintSet.union(ConstraintSet(Constraint(tv2, TFun(tpar, tv1)), Constraint(tv1, trb.t)), trb.cs)
        TypeRelation(tv2, cs, trb.env)
      }
      case With(id, tv, value, body) => {
        val nexp = Apply(Fun(tv, id, body), value)
        check_type(nexp, env)
      }
      case Apply(id, arg) => {
        val trid = check_type(id, env)
        val trarg = check_type(arg, trid.env)
        val tv = getFreshVar
        val cs = ConstraintSet.union(ConstraintSet(Constraint(trid.t, TFun(trarg.t, tv))), trid.cs, trarg.cs)
        TypeRelation(tv, cs, trarg.env)
      }
      case Record(fields) => {
        var ft = Set[FieldType]()
        var constraints = List[ConstraintSet]()
        val cs = ConstraintSet(fields.map((rp: RecPair) => {
          val v = getFreshVar
          val t = check_type(rp.e, env)
          ft = ft + FieldType(rp.s, getFlagVar(rp.s), t.t)
          constraints = constraints :+ t.cs
          Constraint(v,t.t)
        }):_*)
        val csRet = ConstraintSet.union(constraints :+ cs:_*)
        TypeRelation(Pi(ft, Absent(fields.length)), csRet, env)
      }
      case RecordAccess(e, s) =>
        val tr = check_type(e, env)
        val alpha = getFreshVar
        val tau = tr.t match {
          case Pi(fields, abs) =>
            Pi(fields.map((ft : FieldType) => if (ft.name == s) FieldType(ft.name,ft.fieldFlag ,alpha) else FieldType(ft.name, ft.fieldFlag, getFreshVar)), abs)
          case _ => Pi(Set(FieldType(s, getFlagVar(s), alpha)), Absent(1))
        }
        val cs = ConstraintSet.union(ConstraintSet(Constraint(tr.t, tau)), tr.cs)
        TypeRelation(alpha, cs, tr.env)
    }
  }

  private def search_rec(in : Type, looking_for : Type, newType : Type) : Type = {
    in match {
      case TFun(targ, tbody) => TFun(search_rec(targ, looking_for, newType), search_rec(tbody, looking_for, newType))
      case Pi(fields, abs) =>
        Pi(fields.map((ft: FieldType) => {
          if (ft.t == looking_for) FieldType(ft.name, ft.fieldFlag, newType)
          else FieldType(ft.name, ft.fieldFlag, search_rec(ft.t, looking_for, newType))
        }), abs)
      case _ =>
        if (in == looking_for) newType
        else in
    }
  }


  def queryType(tv: Type, subst: Substitution) : Type = {
    subst match {
      case ComplexSubstitution(s1, s2) =>
        queryType(queryType(tv, s2), s1)
        queryType(tv, s1)
      case EmptySubstitution() => tv
      case SimpleSubstitution(t1, t2) => search_rec(tv, t1, t2)
    }
  }

  def best_type(t1: Type, t2: Type) : Type = {
    (t1, t2) match {
      case (TFun(targ1, tbody1), TFun(targ2, tbody2)) => TFun(best_type(targ1, targ2), best_type(tbody1, tbody2))
      case (Pi(_, abs1), Pi(_, abs2)) => if (abs1.nfields > abs2.nfields) t1 else t2
      case (TVar(_), _) => t2
      case (_, TVar(_)) => t1
      case _ => t1
    }

  }

  def hasTypeVariables(t: Type) : Boolean =  {
    t match {
      case TFun(targ, tbody) => hasTypeVariables(targ) || hasTypeVariables(tbody)
      case Pi(fields, abs) => fields.filter(ft => hasTypeVariables(ft.t)).toList.nonEmpty
      case TVar(_) => true
      case _ => false
    }
  }

  def substituteInConstraintSet(oldType: Type, newType: Type, cs: ConstraintSet, tvar : TVar = TVar(-1)) : ConstraintSet = {
    cs match {
      case aConstraintSet(c, cs1) => (c.t1, c.t2) match {
        case (x @ TVar(i), r1 @ Pi(_, _)) =>
          newType match {
            case r2 @ Pi(_,_) =>
              if (tvar.index != -1 && x == tvar) {
                val fv = getFreshVar
                val c1 = Constraint(tvar, fv)
                val c2 = Constraint(fv, mixRecords(r1, r2))
                ConstraintSet.union(ConstraintSet(c2, c1), substituteInConstraintSet(oldType, newType, cs1, x))
              }
              else aConstraintSet(Constraint(substituteInType(c.t1, oldType, newType), substituteInType(c.t2, oldType, newType)),
                substituteInConstraintSet(oldType, newType, cs1, x))
            case _ => aConstraintSet(Constraint(substituteInType(c.t1, oldType, newType), substituteInType(c.t2, oldType, newType)),
                                      substituteInConstraintSet(oldType, newType, cs1, tvar))
          }
        case _=> aConstraintSet(Constraint(substituteInType(c.t1, oldType, newType), substituteInType(c.t2, oldType, newType)),
                                substituteInConstraintSet(oldType, newType, cs1, tvar))
      }
      case EmptyConstraintSet() => EmptyConstraintSet()
    }
  }

  def substituteInType(in: Type, oldType: Type, newType: Type) : Type = {
    in match {
      case TFun(targ, tbody) =>
        TFun(substituteInType(targ, oldType, newType), substituteInType(tbody, oldType, newType))
      case Pi(fields, abs) =>
        Pi(fields.map(ft => FieldType(ft.name, ft.fieldFlag, substituteInType(ft.t, oldType, newType))), abs)
      case _ => if (in == oldType) newType else in
    }
  }

  def mixRecords(t1: Pi, t2: Pi) : Pi = {
    Pi(t1.tfields union t2.tfields, Absent(t1.abs.nfields + t2.abs.nfields))
  }

  def unify(cs: ConstraintSet) : Substitution = {
    cs match {
      case aConstraintSet(c, cs1) =>
        c match {
          case Constraint(t1, t2) => t1 match {
            case TVar(i) => t2 match {
              case Pi(_,_) => ComplexSubstitution(unify(substituteInConstraintSet(t1,t2,cs1,TVar(i))), SimpleSubstitution(t1, t2))
              case _ => ComplexSubstitution(unify(substituteInConstraintSet(t1,t2,cs1)), SimpleSubstitution(t1, t2))
            }
            case TFun(t3, t4) => t2 match {
              case TFun(t5, t6) => unify(ConstraintSet.union(cs1, ConstraintSet(Constraint(t3, t5), Constraint(t4, t6))))
              case TVar(i) => ComplexSubstitution(unify(substituteInConstraintSet(t2,t1,cs1)), SimpleSubstitution(t2, t1))
            }
            case r1 @ Pi(fields1, abs)  => t2 match {
              case r2 @ Pi(fields2, abs) =>
                val fieldToField = ConstraintSet(fields1.zip(fields2).map(trpp => Constraint(trpp._1.t, trpp._2.t)).toSeq:_*)
                unify(ConstraintSet.union(cs1, fieldToField))
              case TVar(i) => ComplexSubstitution(unify(substituteInConstraintSet(t2, t1, cs1)), SimpleSubstitution(t2, t1))
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
      val tr2 = check_type(n2, tr1.env)
      val cs1 = tr1.cs
      val cs2 = tr2.cs
      TypeRelation(TBool(), ConstraintSet.union(ConstraintSet(Constraint(tr1.t, TNum()), Constraint(tr2.t, TNum())), cs1, cs2), tr2.env)
  }

  def check_AE_type(n1: Expr, n2: Expr, env: Env) : TypeRelation = {
    val tr1 = check_type(n1, env)
    val tr2 = check_type(n2, tr1.env)
    val cs1 = tr1.cs
    val cs2 = tr2.cs
    TypeRelation(TNum(), ConstraintSet.union(ConstraintSet(Constraint(tr1.t, TNum()), Constraint(tr2.t, TNum())), cs1, cs2), tr2.env)
  }

  def typeof(expr: Expr) : Type = {
    val type_and_constraints = TypeChecker.check_type(expr)
    val constraints_unified = TypeChecker.unify(type_and_constraints.cs)
    println(type_and_constraints)
    println(constraints_unified)
    val type_resolved = TypeChecker.queryType(type_and_constraints.t, constraints_unified)
    clean_type(type_resolved)

  }

  private def clean_type(t: Type) : Type = {
    t match {
      case Pi(fields, abs) => TRecord(fields.map(ft => TField(ft.name, ft.t)))
      case TFun(targ, tbody) => TFun(clean_type(targ), clean_type(tbody))
      case _ => t
    }
  }

  def env_lookup (env : Env, s : Symbol) : Any = {
    env match {
      case aEnv(sy, v, e) => {
        if (s == sy) v
        else env_lookup(e, s)
      }
      case EmptyEnv() => {
        println(s"Variable $s not defined in scope")
        sys.exit()
      }
    }
  }
}