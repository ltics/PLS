/**
 * Created by zjh on 15/11/10.
 */

import lisp.{EnvMapT, EnvT, ExprT}

import scala.collection.immutable._

package object lisp {
    type EnvMapT = HashMap[String, ExprT]
    type EnvT = List[EnvMapT]

    def EnvT() = List(new EnvMapT())

    def EnvT(xs: EnvMapT*) = List(xs: _*)

    def EnvMapT(xs: (String, ExprT)*) = HashMap(xs: _*)
}

case class Env(val env: EnvT) {
    def addScope(scope: (String, ExprT)): Env = env match {
        case h :: t => Env((h + scope) :: t)
        case List() => throw new IllegalArgumentException
    }

    def expand(): Env = Env(new EnvMapT() :: env)

    def lookup(s: String): Option[ExprT] = {
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
