package edu.nus.comp.pli.simpl.util

import edu.nus.comp.pli.simpl.parser.SimplAST._
import edu.nus.comp.pli.simpl.util.FreeVariables._
import edu.nus.comp.pli.simpl.util.SimplEvmInstruction._


object SimplEplDenotationalSemantics {
  private var labal_index = -1

  private def fresh_labal() : String ={
    labal_index = labal_index + 1
    "labal_"+ labal_index
  }

  private def get_val (ce:Seq[(Var,Int)], s:String) :Option[Int] = ce match{
    case Seq() => None
    case head::rest => head match{
      case (Var(name),index) => if (name == s) Some(index) else get_val(rest, s)
    }
  }

  private def trans_bin_cmd (op:BinaryOperator):SimInstruction = op match {
    case And => AND
    case Or => OR
    case Mul => TIMES
    case Div => DIV
    case Add => PLUS
    case Sub => MINUS
    case Lt => LT
    case Lte => LE
    case Gt => GT
    case Gte => GE
    case Eq => EQ
    case NEq => NEQ
  }
  private def trans_ury_cmd (op:UnaryOperator):SimInstruction = op match {
    case Not => NOT
    case Minus => NEG
  }

  private def  enum_cenv (xss:List[Var], nn:Int):Seq[(Var,Int)] ={
    def aux (xs:List[Var], n:Int):Seq[(Var,Int)] = xs match {
      case Nil => Seq()
      case head :: rest => (head,n) +: aux (rest, n+1)
    }
    aux (xss,nn)
  }

  private def compileHelper(ce:Seq[(Var,Int)],expr: Expression): (Seq[SimInstruction],Seq[SimInstruction])  = expr match {
    case Num(i) => (Seq(LDCI (i)),Seq())
    case Bool(b) => (Seq(LDCB (b)),Seq())

    case Var(v) => get_val (ce,v) match {
      case Some (i) => (Seq(LD (v,i)),Seq())
      case None => (Seq(LD (v,-1)),Seq())
    }

    case Bin(op, leftExpr, rightExpr) =>
      val (s1,p1) = compileHelper (ce, leftExpr)
      val (s2,p2) = compileHelper (ce, rightExpr)

      (s1++s2++Seq(trans_bin_cmd(op)),p1++p2)

    case Ury(op, expression) =>
      val (s,p) = compileHelper(ce, expression)
      (s++Seq(trans_ury_cmd (op)),p)


    case Func(t,vs,body) =>
      val l_fn = fresh_labal()
      val fvs = fv (expr)
      val all_vs = fvs ++ vs
      val new_ce = enum_cenv (all_vs.toList, 0)
      val arity = vs.length
      val (s1,p1) = compileHelper( new_ce, body)
      val fvs_n = fvs.map(
        (ele:Var) => ele match {
          case Var (name) => get_val(ce,name) match {
            case Some (i) => (name,i)
            case None => (name,-1)
          }
        }

      )
      (Seq(LDF(fvs_n.toSeq,arity,l_fn)), (LABEL(l_fn)+:s1)++Seq(RTN)++p1)

    case Cond(condition, consequent, alternative) =>
      // add you code here
      val l_fn_1 = fresh_labal()
      val l_fn_2 = fresh_labal()
      val (s1, p1) = compileHelper(ce, condition)
      val (s2, p2) = compileHelper(ce, consequent)
      val (s3, p3) = compileHelper(ce, alternative)

      ((((s1 :+ JOF(l_fn_1)) ++ s2 :+ GOTO(l_fn_2)) :+ LABEL(l_fn_1)) ++ s3 :+ LABEL(l_fn_2), p1 ++ p2 ++ p3)

    case RecFunc(t,f,vs,body) =>
      // add you code here
      val l_fn = fresh_labal()
      val fvs = fv(expr)
      val all_vs = (f +: fvs.toList) ++ vs
      val new_ce = enum_cenv(all_vs, 0)
      val arity = vs.length
      val (s1, p1) = compileHelper(new_ce, body)
      val fvs_n = fvs.map(
        (ele: Var) => ele match {
          case Var(name) => get_val(new_ce, name) match {
            case Some(i) => (name, i)
            case None => (name, -1)
          }
        }
      )
      val f_n = get_val(new_ce, f.name) match {
        case Some(i) => (f.name, i)
        case None => (f.name, -1)
      }

      (Seq(LDFR(fvs_n.toList,f_n,arity,l_fn)), (LABEL(l_fn)+:s1)++Seq(RTN)++p1)

    case Appln(f,args) =>
      // add you code here
      val arity = args.length
      val (s0, p0) = compileHelper(ce, f)
      val partial = args.map(x => compileHelper(ce, x))
      val s: Seq[SimInstruction] = partial.reverse.flatMap(x => x._1)
      val p: Seq[SimInstruction] = partial.flatMap(x => x._2)
      (s ++ s0 :+ CALL(arity), p0 ++ p)
  }


  def compile(expr: Expression):Seq[SimInstruction] = {
    labal_index = -1

    val (main_code,proc_code) = compileHelper (Seq(), trans_exp(expr))
    tail_call_op(main_code) ++ (DONE +: tail_call_op(proc_code))

  }

  def tail_call_op(main_code: Seq[SimInstruction]): Seq[SimInstruction] = main_code match {
    case CALL(n) :: RTN :: rest => TAILCALL(n) +: tail_call_op(rest)
    case instr :: rest => instr +: tail_call_op(rest)
    case Nil => Seq()
  }

  private def trans_exp (e:Expression) :Expression = e match{
    case Num(i) => Num(i)
    case Bool(b) => Bool(b)
    case Var(va) => Var(va)
    case Bin(op, leftExpr, rightExpr) =>
      Bin(op, trans_exp(leftExpr), trans_exp(rightExpr))

    case Ury(op, expression) =>
      Ury(op, trans_exp (expression))

    case Cond(condition, consequent, alternative) =>
      Cond(trans_exp(condition), trans_exp(consequent), trans_exp(alternative))

    case Func(t,vs,body) =>
      Func(t,vs,trans_exp(body))

    case RecFunc(t,f,vs,body) =>
      RecFunc(t,f,vs,trans_exp(body))

    case Appln(f,args) =>
      Appln(trans_exp(f),args.map(trans_exp))

    case Let (ls,t,body) =>
      def type_builder (lls:Seq[Type], tt:Type):Type = (lls,tt) match {
        case (Seq(t1), _) => Arrow(t1, t)
        case (v :: vs, _) =>
          val nt2 = type_builder(vs, t)
          Arrow(v, nt2)
      }
      val ids = ls.map((ele:(Type, Var, Expression)) => ele._2)
      val expr = ls.map((ele:(Type, Var, Expression)) => trans_exp(ele._3))
      val types = ls.map((ele:(Type, Var, Expression)) => ele._1) //List.map (fun (t, _, _) -> t) ls in
      val body_expr = trans_exp (body)
      val func_type = type_builder (types, t)
      val func = Func(func_type, ids, body_expr)
      Appln(func, expr)

    case _ =>  e

  }

}
