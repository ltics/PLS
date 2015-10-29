package hm

/**
 * Created by zjh on 15/10/28.
 */

abstract class SyntaxNode
case class Lambda(v: String, body: SyntaxNode) extends SyntaxNode
case class Ident(name: String) extends SyntaxNode
case class Apply(fn: SyntaxNode, arg: SyntaxNode) extends SyntaxNode
case class Let(v: String, defn: SyntaxNode, body: SyntaxNode) extends SyntaxNode
case class Letrec(v: String, defn: SyntaxNode, body: SyntaxNode) extends SyntaxNode

object SyntaxNode {
    def string(ast: SyntaxNode): String = {
        if (ast.isInstanceOf[Ident])
            nakedString(ast)
        else
            "("+nakedString(ast)+")"
    }

    def nakedString(ast: SyntaxNode) = ast match {
        case i: Ident => i.name
        case l: Lambda => "fn "+l.v+" ⇒ "+string(l.body)
        case f: Apply => string(f.fn)+" "+string(f.arg)
        case l: Let => "let "+l.v+" = "+string(l.defn)+" in "+string(l.body)
        case l: Letrec => "letrec "+l.v+" = "+string(l.defn)+" in "+string(l.body)
    }
}
