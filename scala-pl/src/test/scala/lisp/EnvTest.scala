package lisp

import org.scalatest._

/**
 * Created by zjh on 15/11/10.
 */

class EnvTest extends FlatSpec with Matchers {
    val scope = "key" -> Value(Num(3))
    val testEnv = Env().addScope(scope)

    it should "add a scope" in {
        val resEnv = Env(EnvT(EnvMapT(scope)))
        resEnv should be (Env().addScope(scope))
    }

    it should "simple lookup" in {
        testEnv lookup "key" should be (Some(Value(Num(3))))
    }
}
