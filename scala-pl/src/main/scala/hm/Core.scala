package hm

/**
 * Created by zjh on 15/10/21.
 */

object Core {

    def main(args: Array[String]){
        Console.setOut(new java.io.PrintStream(Console.out, true, "utf-8"))

        val var1 = TypeSystem.newVariable
        val var2 = TypeSystem.newVariable
        val pairtype = TypeSystem.Oper("×", Array(var1, var2))

        val var3 = TypeSystem.newVariable

        val myenv: TypeSystem.Env = Map.empty ++ Array(
            "pair" -> TypeSystem.Function(var1, TypeSystem.Function(var2, pairtype)),
            "true" -> TypeSystem.Bool,
            "cond" -> TypeSystem.Function(TypeSystem.Bool, TypeSystem.Function(var3, TypeSystem.Function(var3, var3))),
            "zero" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Bool),
            "pred" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer),
            "times"-> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer))
        )


        val pair = Apply(Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))), Apply(Ident("f"), Ident("true")))
        val examples = Array[SyntaxNode](
            // factorial
            Letrec("factorial", // letrec factorial =
                Lambda("n",    // fn n =>
                    Apply(
                        Apply(   // cond (zero n) 1
                            Apply(Ident("cond"),     // cond (zero n)
                                Apply(Ident("zero"), Ident("n"))),
                            Ident("1")),
                        Apply(    // times n
                            Apply(Ident("times"), Ident("n")),
                            Apply(Ident("factorial"),
                                Apply(Ident("pred"), Ident("n")))
                        )
                    )
                ),      // in
                Apply(Ident("factorial"), Ident("5"))
            ),

            // Should fail:
            // fn x => (pair(x(3) (x(true)))
            Lambda("x",
                Apply(
                    Apply(Ident("pair"),
                        Apply(Ident("x"), Ident("3"))),
                    Apply(Ident("x"), Ident("true")))),

            // pair(f(3), f(true))
            Apply(
                Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))),
                Apply(Ident("f"), Ident("true"))),


            // letrec f = (fn x => x) in ((pair (f 4)) (f true))
            Let("f", Lambda("x", Ident("x")), pair),

            // fn f => f f (fail)
            Lambda("f", Apply(Ident("f"), Ident("f"))),

            // let g = fn f => 5 in g g
            Let("g",
                Lambda("f", Ident("5")),
                Apply(Ident("g"), Ident("g"))),

            // example that demonstrates generic and non-generic variables:
            // fn g => let f = fn x => g in pair (f 3, f true)
            Lambda("g",
                Let("f",
                    Lambda("x", Ident("g")),
                    Apply(
                        Apply(Ident("pair"),
                            Apply(Ident("f"), Ident("3"))
                        ),
                        Apply(Ident("f"), Ident("true"))))),

            // Function composition
            // fn f (fn g (fn arg (f g arg)))
            Lambda("f", Lambda("g", Lambda("arg", Apply(Ident("g"), Apply(Ident("f"), Ident("arg"))))))
        )
        for(eg <- examples){
            tryexp(myenv, eg)
        }
    }

    def tryexp(env: TypeSystem.Env, ast: SyntaxNode) {
        print(SyntaxNode.string(ast) + " : ")
        try {
            val t = TypeSystem.analyse(ast, env)
            print(TypeSystem.string(t))

        }catch{
            case t: ParseError => print(t.getMessage)
            case t: TypeError => print(t.getMessage)
        }
        println
    }
}
