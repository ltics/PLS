package lisp

import scala.util.parsing.combinator._

/**
 * Created by zjh on 15/11/9.
 */
object Parser extends JavaTokenParsers {
    val value: Parser[ValueT] = {
        //第一个callback里面先tail再init是为了去除两边的空格
        stringLiteral ^^ (x => Name(x.tail.init)) |
        floatingPointNumber ^^ (x => Num(BigDecimal(x)))
    }

    val expression: Parser[ExprT] = {
        value ^^ (x => Value(x)) |
        """[^()\s]+""".r ^^ (x => Symbol(x)) |
        combination
    }

    val combination: Parser[Comb] = "(" ~> rep(expression) <~ ")" ^^ (x => Comb(x))

    //不管source内容是啥最后都会包在一个List里面
    val program: Parser[List[ExprT]] = rep(expression)

    def parse(source: String) = parseAll(program, source).get
}
