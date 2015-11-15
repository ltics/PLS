package lisp

import lisp.Interp._

/**
 * Created by zjh on 15/11/15.
 */
object BuiltIn {
    //defina partial func in scala
    def arithFun(op: ((BigDecimal, BigDecimal) => BigDecimal))(env: Env, comp: List[ExprT]) = {
        def error = throw new IllegalArgumentException("arithmetic error")
        comp.map(eval(env, _)._2) match {
            case Value(Num(first)) :: t => {
                val res = t.foldLeft(first)((acc, e) => {
                    e match {
                        case Value(Num(v)) => op(acc, v)
                        case _             => error
                    }
                })
                (env, Value(Num(res)))
            }
            case _                      => error
        }
    }

    def compFun(op: ((BigDecimal, BigDecimal) => Boolean))(env: Env, comp: List[ExprT]) = {
        def error = throw new IllegalArgumentException("comparison error")
        comp.map(eval(env, _)._2) match {
            case Value(Num(first)) :: t => {
                val res = t.foldLeft(true, first)((acc, e) => {
                    e match {
                        case Value(Num(v)) => (acc._1 && op(acc._2, v), v)
                        case _             => error
                    }
                })
                (env, Value(Bool(res._1)))
            }
            case _                      => error
        }
    }

    def globalEnv = Env(EnvT(EnvMapT()))
}
