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

    it should "parse multiple token without brackets" in {
        parse("3 3") should be (List(Value(Num(3.0)), Value(Num(3.0))))
    }

    it should "parse s-expressions" in {
        parse("(+ 3 3)") should be (List(Comb(List(Symbol("+"), Value(Num(3.0)), Value(Num(3.0))))))
    }
}
