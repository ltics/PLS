package hm

/**
 * Created by zjh on 15/10/28.
 */

object TypeSystem {

    type Env = Map[String, Type]

    abstract class Type
    case class Variable(id: Int) extends Type {
        var instance: Option[Type] = None
        lazy val name = nextUniqueName
    }
    case class Oper(name: String, args: Seq[Type]) extends Type
    def Function(from: Type, to: Type) = Oper("→", Array(from, to))
    val Integer = Oper("int", Seq())
    val Bool = Oper("bool", Seq())

    var _nextVariableName = 'α';
    def nextUniqueName = {
        val result = _nextVariableName
        _nextVariableName = (_nextVariableName.toInt + 1).toChar
        result.toString
    }
    var _nextVariableId = 0
    def newVariable: Variable = {
        val result = _nextVariableId
        _nextVariableId += 1
        Variable(result)
    }

    def string(t: Type): String = t match {
        case v: Variable => v.instance match {
            case Some(i) => string(i)
            case None => v.name
        }
        case Oper(name, args) => {
            if (args.length == 0)
                name
            else if (args.length == 2)
                "("+string(args(0))+" "+name+" "+string(args(1))+")"
            else
                args.mkString(name + " ", " ", "")
        }
    }


    def analyse(ast: SyntaxNode, env: Env): Type = analyse(ast, env, Set.empty)
    def analyse(ast: SyntaxNode, env: Env, nongen: Set[Variable]): Type = ast match {
        case Ident(name) => gettype(name, env, nongen)
        case Apply(fn, arg) => {
            val funtype = analyse(fn, env, nongen)
            val argtype = analyse(arg, env, nongen)
            val resulttype = newVariable
            unify(Function(argtype, resulttype), funtype)
            resulttype
        }
        case Lambda(arg, body) => {
            val argtype = newVariable
            val resulttype = analyse(body,
                env + (arg -> argtype),
                nongen + argtype)
            Function(argtype, resulttype)
        }
        case Let(v, defn, body) => {
            val defntype = analyse(defn, env, nongen)
            val newenv = env + (v -> defntype)
            analyse(body, newenv, nongen)
        }
        case Letrec(v, defn, body) => {
            val newtype = newVariable
            val newenv = env + (v -> newtype)
            val defntype = analyse(defn, newenv, nongen + newtype)
            unify(newtype, defntype)
            analyse(body, newenv, nongen)
        }
    }

    def gettype(name: String, env: Env, nongen: Set[Variable]): Type = {
        if (env.contains(name))
            fresh(env(name), nongen)

        else if (isIntegerLiteral(name))
            Integer

        else
            throw new ParseError("Undefined symbol "+name)
    }

    def fresh(t: Type, nongen: Set[Variable]) = {
        import scala.collection.mutable
        val mappings = new mutable.HashMap[Variable, Variable]
        def freshrec(tp: Type): Type = {
            prune(tp) match {
                case v: Variable =>
                    if (isgeneric(v, nongen))
                        mappings.getOrElseUpdate(v, newVariable)
                    else
                        v

                case Oper(name, args) =>
                    Oper(name, args.map(freshrec(_)))
            }
        }

        freshrec(t)
    }



    def unify(t1: Type, t2: Type) {
        val type1 = prune(t1)
        val type2 = prune(t2)
        (type1, type2) match {
            case (a: Variable, b) => if (a != b) {
                if (occursintype(a, b))
                    throw new TypeError("recursive unification")
                a.instance = Some(b)
            }
            case (a: Oper, b: Variable) => unify(b, a)
            case (a: Oper, b: Oper) => {
                if (a.name != b.name ||
                    a.args.length != b.args.length) throw new TypeError("Type mismatch: "+string(a)+"≠"+string(b))

                for(i <- 0 until a.args.length)
                    unify(a.args(i), b.args(i))
            }
        }
    }


    // Returns the currently defining instance of t.
    // As a side effect, collapses the list of type instances.
    def prune(t: Type): Type = t match {
        case v: Variable if v.instance.isDefined => {
            var inst = prune(v.instance.get)
            v.instance = Some(inst)
            inst
        }
        case _ => t
    }

    // Note: must be called with v 'pre-pruned'
    def isgeneric(v: Variable, nongen: Set[Variable]) = !(occursin(v, nongen))

    // Note: must be called with v 'pre-pruned'
    def occursintype(v: Variable, type2: Type): Boolean = {
        prune(type2) match {
            case `v` => true
            case Oper(name, args) => occursin(v, args)
            case _ => false
        }
    }

    def occursin(t: Variable, list: Iterable[Type]) =
        list exists (t2 => occursintype(t, t2))

    val checkDigits = "^(\\d+)$".r
    def isIntegerLiteral(name: String) = checkDigits.findFirstIn(name).isDefined

}
