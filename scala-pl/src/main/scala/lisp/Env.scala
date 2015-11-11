/**
 * Created by zjh on 15/11/10.
 */

package object lisp {
    type EnvMapT = Map[String, ExprT]
    type EnvT = List[EnvMapT]

    def EnvT() = List(EnvMapT())
    def EnvT(xs: EnvMapT*) = List(xs: _*)
    def EnvMapT(xs: (String, ExprT)*) = Map(xs: _*)

    case class Env(val env: EnvT) {
        //+用来append Map
        //::用来cons List
        def addScope(scope: (String, ExprT)): Env = env match {
            case h :: t => Env((h + scope) :: t)
            case List() => throw new IllegalArgumentException
        }

        def expand(): Env = Env(EnvMapT() :: env)

        def lookup(s: String): Option[ExprT] = {
            //find是从一个collection里找另一个collection 返回的是一个Option
            //下面这个map是Option的map 也就是Maybe这个functor上的fmap
            env find (_ contains s) map (_(s))
        }

        override def equals(that: Any) = that match {
            case Env(thatEnv) => env == thatEnv
            case _ => false
        }
    }

    object Env {
        def apply() = new Env(EnvT())
    }
}
