package edu.nus.comp.pli.simpl.util

import edu.nus.comp.pli.simpl.parser.SimplAST._

object FreeVariables {
  def fv(expr: Expression): Set[Var] = {
    expr match {
      case Num(_) => Set()
      case Bool(_) => Set()
      case variable @ Var(v) => Set(variable)

      case Bin(_, leftExpr, rightExpr) => fv(leftExpr) union fv(rightExpr)

      case Ury(_, expression) => fv(expression)

      case Cond(condition, consequent, alternative) =>
        fv(condition) union fv(consequent) union fv(alternative)

      case Func(_, formalArgs, body) =>
        fv(body) // add you code here

      case RecFunc(typ, name, formalArgs, body) =>
        fv(body) // add you code here

      case Appln(func, actualArgs) =>
        fv(func)  // add you code here

      case Let(defs, _, body) =>
        fv(body)  // add you code here
    }
  }
}
