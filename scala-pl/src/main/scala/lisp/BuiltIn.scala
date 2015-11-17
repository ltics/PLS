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
        "(" + ltos(ls) + ")"
    }

    def _not_(env: Env, comb: List[ExprT]) = comb match {
        case expr :: Nil => eval(env, expr) match {
            case (_, Value(Bool(v))) => (env, Value(Bool(!v)))
            case _                   => throw new IllegalArgumentException("not")
        }
        case _           => throw new IllegalArgumentException("not")
    }

    def _if_(env: Env, comb: List[ExprT]) = {
        def error = throw new IllegalArgumentException("if")
        val (condExpr, posExpr, negExpr) = comb match {
            case predicate :: consequent :: alternative :: Nil => (predicate, consequent, Some(alternative))
            case predicate :: consequent :: Nil                => (predicate, consequent, None)
            case _                                             => error
        }

        eval(env, condExpr)._2 match {
            case Value(Bool(c)) =>
                if (c) {
                    eval(env, posExpr)
                } else negExpr match {
                    case Some(e) => eval(env, e)
                    case None    => (env, NullExpr())
                }
            case _              => error
        }
    }

    def _cond_(env: Env, comb: List[ExprT]) = {
        def error = throw new IllegalArgumentException("cond")
        def doExpr(comb: List[ExprT]) = comb match {
            case Symbol("else") :: posExpr :: Nil => Some(eval(env, posExpr)._2)
            case condExpr :: posExpr :: Nil       => eval(env, condExpr)._2 match {
                case Value(Bool(true))  => Some(eval(env, posExpr)._2)
                case Value(Bool(false)) => None
                case _                  => error
            }
            case _                                => error
        }

        def runExprs(comb: List[ExprT]): ExprT = comb match {
            case Comb(c) :: rest => doExpr(c) match {
                case Some(e) => e
                case None    => runExprs(rest)
            }
            case _               => NullExpr()
        }
    }

    def globalEnv = Env(EnvT(EnvMapT()))
}
