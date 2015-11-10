import org.scalatest._

/**
 * Created by zjh on 15/11/10.
 */

class EnvTest extends FlatSpec with Matchers {
    it should "be three" in {
        3 should be (3)
    }
}
