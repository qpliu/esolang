import java.io.File;
import java.io.PrintWriter;
import java.util.HashSet;
import java.util.Map;

public class Function {
    private Def[] defs;

    public Function(Def[] defs) {
        this.defs = defs;
    }

    public int getArity() {
        return defs[0].getArity();
    }

    public void parse(Map<String,Function> functions) {
        for (Def def : defs)
            def.parse(functions);
    }

    private class Result extends rt01_.function {
        private rt01_.val[] args;
        Result(rt01_.val[] args) {
            this.args = args;
        }

        protected rt01_.val eval() {
            for (Def def : defs) {
                rt01_.val val = def.eval(args);
                if (val != null) {
                    args = null; // let arguments get garbage collected
                    return val;
                }
            }
            Token name = defs[defs.length-1].getName();
            throw new RuntimeException(name.getLocation() + ": no matching pattern in definition of " + name.getSymbol());
        }
    }

    public rt01_.function eval(rt01_.val[] args) {
        return new Result(args);
    }

    private static final HashSet<String> reserved = new HashSet<String>();
    static {
        reserved.add("abstract");
        reserved.add("assert");
        reserved.add("boolean");
        reserved.add("break");
        reserved.add("byte");
        reserved.add("case");
        reserved.add("catch");
        reserved.add("char");
        reserved.add("class");
        reserved.add("const");
        reserved.add("continue");
        reserved.add("default");
        reserved.add("do");
        reserved.add("double");
        reserved.add("else");
        reserved.add("enum");
        reserved.add("extends");
        reserved.add("false");
        reserved.add("final");
        reserved.add("finally");
        reserved.add("float");
        reserved.add("for");
        reserved.add("goto");
        reserved.add("if");
        reserved.add("implements");
        reserved.add("import");
        reserved.add("instanceof");
        reserved.add("int");
        reserved.add("interface");
        reserved.add("long");
        reserved.add("native");
        reserved.add("new");
        reserved.add("null");
        reserved.add("package");
        reserved.add("private");
        reserved.add("protected");
        reserved.add("public");
        reserved.add("return");
        reserved.add("short");
        reserved.add("static");
        reserved.add("switch");
        reserved.add("synchronized");
        reserved.add("strictfp");
        reserved.add("super");
        reserved.add("this");
        reserved.add("throw");
        reserved.add("throws");
        reserved.add("transient");
        reserved.add("true");
        reserved.add("try");
        reserved.add("void");
        reserved.add("volatile");
        reserved.add("while");
    }

    public static String mangle(String name) {
        StringBuilder sb = new StringBuilder();
        if (reserved.contains(name))
            sb.append("__");
        for (int i = 0; i < name.length(); i++)
            switch (name.charAt(i)) {
            case '0': case '1': case '2': case '3': case '4': case '5':
            case '6': case '7': case '8': case '9':
                if (i == 0)
                    sb.append("__");
            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
            case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
            case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
            case 's': case 't': case 'u': case 'v': case 'w': case 'x':
            case 'y': case 'z':
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
            case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
            case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
            case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
            case 'Y': case 'Z':
                sb.append(name.charAt(i));
                break;
            default:
                sb.append('_').append(Integer.toHexString(name.charAt(i))).append('_');
            }
        return sb.toString();
    }

    public String getMangledName() {
        return mangle(defs[0].getName().getSymbol());
    }

    public void compile(PrintWriter out) throws Exception {
        int arity = getArity();
        String name = getMangledName();
        out.print("public class ");
        out.print(name);
        out.println(" extends rt01_.function {");
        for (int i = 0; i < arity; i++) {
            out.print("    private rt01_.val a");
            out.print(i);
            out.println(";");
        }
        out.print("    public ");
        out.print(name);
        out.print("(");
        for (int i = 0; i < arity; i++) {
            if (i > 0)
                out.print(",");
            out.print("rt01_.val p");
            out.print(i);
        }
        out.println(") {");
        for (int i = 0; i < arity; i++) {
            out.print("        a");
            out.print(i);
            out.print("=p");
            out.print(i);
            out.println(";");
        }
        out.println("    }");
        out.println("    protected rt01_.val eval() {");
        out.println("        rt01_.val val;");
        out.print("        if (");
        for (int i = 0; i < defs.length; i++) {
            out.print("(val = m");
            out.print(i);
            out.print("()) == null && ");
        }
        out.print("true) throw new RuntimeException(\"");
        out.print(getLocation(defs[defs.length-1].getName()));
        out.println(": pattern match failed\");");
        for (int i = 0; i < arity; i++) {
            out.print("        a");
            out.print(i);
            out.println(" = null;");
        }
        out.println("        return val;");
        out.println("    }");
        HashSet<String> constants = new HashSet<String>();
        for (Def def : defs)
            collectConstants(constants, def.getExpr());
        for (String constant : constants) {
            out.print("    private static final rt01_.val _");
            out.print(constant);
            if (constant.length() == 0) {
                out.println(" = NIL;");
            } else {
                out.print(" = new rt01_.constant(\"");
                out.print(constant);
                out.println("\");");
            }
        }
        for (int i = 0; i < defs.length; i++)
            defs[i].compile(i, out);
        out.println("    public static void main(String[] args) throws Exception {");
        out.print("        main(args,");
        out.print(arity);
        out.print(",\"");
        out.print(name);
        out.println("\");");
        out.println("    }");
        out.println("}");
    }

    private void collectConstants(HashSet<String> constants, Expr expr) {
        if (expr instanceof ConstantExpr) {
            constants.add(((ConstantExpr) expr).getBits());
        } else if (expr instanceof ConcatExpr) {
            collectConstants(constants, ((ConcatExpr) expr).getFirst());
            collectConstants(constants, ((ConcatExpr) expr).getSecond());
        } else if (expr instanceof FuncallExpr) {
            for (Expr arg : ((FuncallExpr) expr).getArgs())
                collectConstants(constants, arg);
        }
    }

    public static String getLocation(Token token) {
        return new File(token.getFileName()).getName() + ":" + token.getLineNumber() + ":" + token.getColumn();
    }
}
