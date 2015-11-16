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

    def buildList(comb: List[ExprT]): List[ValueT] = comb match {
        case List()         => List()
        case Symbol(n) :: t => Name(n) :: buildList(t)
        case Value(v) :: t  => v :: buildList(t)
        case _              => throw new IllegalArgumentException("define args")
    }

    def listToString(ls: List[ExprT]) = {
        def ltos(ls: List[ExprT]): String = ls match {
            case List()              => ""
            case Value(Num(v)) :: t  => v.toString + ", " + ltos(t)
            case Value(Bool(v)) :: t => v.toString + ", " + ltos(t)
            case Value(Name(v)) :: t => v.toString + ", " + ltos(t)
            case EList(l) :: t       => "(" + ltos(l) + "), " + ltos(t)
            case _ :: t              => ltos(t)
        }
    }

    def globalEnv = Env(EnvT(EnvMapT()))
}
