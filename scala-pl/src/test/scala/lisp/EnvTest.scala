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

    it should "nested lookup" in {
        val newEnv = testEnv.expand()
        newEnv lookup "key" should be (Some(Value(Num(3))))
    }

    it should "shadowed nested lookup" in {
        val newEnv = testEnv.expand().addScope("key" -> Value(Num(33)))
        newEnv.lookup("key") should be (Some(Value(Num(33))))
    }

    it should "failing lookup" in {
        testEnv.lookup("newkey") should be (None)
    }
}
