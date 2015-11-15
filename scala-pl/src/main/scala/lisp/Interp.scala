package lisp

/**
 * Created by zjh on 15/11/11.
 */
object Interp {
    def eval(env: Env, expr: ExprT): (Env, ExprT) = expr match {
        case NullExpr()       => throw new IllegalStateException("invalid interpreter state")
        case Comb(List())     => throw new IllegalStateException("invalid combination")
        case Comb(h :: t)     => {
            //t maybe an empty list
            eval(env, h) match {
                case (_, Proc(f))             => apply(f, t, env)
                case (nEnv, Func(args, body)) => {
                    if (args.length != t.length) throw new IllegalArgumentException("invalid number of arguments")
                    val newEnv = (args zip t).foldLeft(nEnv.expand())((acc, av) => bindArg(acc, av._1, av._2))
                    evalAll(newEnv, body)
                }
            }
        }
        case p @ Proc(_)      => (env, p)
        case Func(args, body) => throw new IllegalArgumentException("invalid function call")
        case v @ Value(_)     => (env, v)
        case l @ EList(_)     => (env, l)
        case Symbol(s)        => {
            env.lookup(s) match {
                case Some(v) => (env, v)
                case None    => throw new IllegalArgumentException("unbound symbol '" + s + "'")
            }
        }
    }

    private def apply(f: ((Env, List[ExprT]) => (Env, ExprT)), args: List[ExprT], env: Env) = {
        f(env, args)
    }

    //just bind a new argument in a new inner env
    private def bindArg(env: Env, arg: ValueT, expr: ExprT) = arg match {
        case Name(n) => env.addScope(n -> eval(env, expr)._2)
        case _       => throw new IllegalArgumentException
    }

    def evalAll(env: Env, comb: List[ExprT]): (Env, ExprT) = comb match {
        case List() => (env, NullExpr())
        case h :: t => {
            val (nEnv, res) = eval(env, h)
            //eval一个list的form只返回最后一个form的值
            t.length match {
                case 0 => (env, res)
                case 1 => eval(nEnv, t.head)
                case _ => evalAll(nEnv, t)
            }
        }
    }
}
