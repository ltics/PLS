package lisp

import org.scalatest._
import lisp.Parser.parse

/**
 * Created by zjh on 15/11/11.
 */
class ParserTest extends FlatSpec with Matchers {
    it should "parse literal string" in {
        parse("(def clea \"clea\")") should be (List(Comb(List(Symbol("def"), Symbol("clea"), Value(Name("clea"))))))
        parse("\"clea\"") should be (List(Value(Name("clea"))))
    }
}
