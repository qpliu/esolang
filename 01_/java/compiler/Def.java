import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Map;

public class Def {
    private Token name;
    private Pattern[] patterns;
    private Token[] body;

    private int bindingCount;
    private Expr expr;

    public Def(Token name, Pattern[] patterns, Token[] body) {
        this.name = name;
        this.patterns = patterns;
        this.body = body;

        bindingCount = 0;
        for (Pattern pattern : patterns)
            if (pattern.isBinding())
                bindingCount++;
    }

    public int getArity() {
        return patterns.length;
    }

    public Token getName() {
        return name;
    }

    private class ParseState {
        int index;
        Expr expr;
    }

    public void parse(Map<String,Function> functions) {
        if (body.length == 0) {
            expr = new ConstantExpr(name, new boolean[0]);
        } else {
            ParseState state = new ParseState();
            state.index = 0;
            state.expr = null;
            parse(functions, state);
            expr = state.expr;
        }
        body = null;
    }

    private void parse(Map<String,Function> functions, ParseState state) {
        state.expr = null;
        parse1(functions, state);
        if (state.index < body.length) {
            Expr first = state.expr;
            parse(functions, state);
            state.expr = new ConcatExpr(first, state.expr);
        }
    }

    private void parse1(Map<String,Function> functions, ParseState state) {
        Token token = body[state.index];
        switch (token.getType()) {
        case EQUALS: case DOT:
            assert false;
            throw new RuntimeException();
        case ZERO: case ONE: case NIL:
            parseConstant(token, state);
            return;
        case SYMBOL:
            state.index++;
            state.expr = binding(token);
            if (state.expr != null)
                return;
            if (!functions.containsKey(token.getSymbol()))
                throw new RuntimeException(token.getLocation() + ": unknown function: " + token.getSymbol());
            Function function = functions.get(token.getSymbol());
            Expr[] args = new Expr[function.getArity()];
            for (int i = 0; i < args.length; i++) {
                parse1(functions, state);
                args[i] = state.expr;
            }
            state.expr = new FuncallExpr(token, function, args);
        }
    }

    private void parseConstant(Token token, ParseState state) {
        ArrayList<Boolean> bits = new ArrayList<Boolean>();
        loop: while (state.index < body.length) {
            switch (body[state.index].getType()) {
            case ZERO:
                state.index++;
                bits.add(false);
                break;
            case ONE:
                state.index++;
                bits.add(true);
                break;
            case NIL:
                state.index++;
            default:
                break loop;
            }
        }
        state.expr = new ConstantExpr(token, DefReader.toBitArray(bits));
    }

    private BoundExpr binding(Token token) {
        int bindingIndex = 0;
        for (Pattern pattern : patterns) {
            if (!pattern.isBinding())
                continue;
            if (pattern.getToken().getSymbol().equals(token.getSymbol()))
                return new BoundExpr(token, bindingIndex);
            bindingIndex++;
        }
        return null;
    }

    public Expr getExpr() {
        return expr;
    }

    public rt01_.val eval(rt01_.val[] args) {
        assert args.length == patterns.length;
        rt01_.val[] bindings = new rt01_.val[bindingCount];
        int bindingIndex = 0;
        for (int i = 0; i < args.length; i++) {
            rt01_.val val = patterns[i].match(args[i]);
            if (val == null)
                return null;
            if (patterns[i].isBinding())
                bindings[bindingIndex++] = val;
        }
        return expr.eval(bindings);
    }

    public void compile(int n, PrintWriter out) throws Exception {
        out.print("    private rt01_.val m");
        out.print(n);
        out.println("() {");
        if (patterns.length > 0)
            out.println("        rt01_.val val;");
        int bindingIndex = 0;
        for (int i = 0; i < patterns.length; i++) {
            Pattern pattern = patterns[i];
            boolean[] bits = pattern.getBits();
            if (bits.length > 0) {
                out.print("        a");
                out.print(i);
                out.print(" = trampoline(a");
                out.print(i);
                out.println(");");
            }
            out.print("        val = a");
            out.print(i);
            out.println(";");
            boolean start = true;
            for (boolean bit : bits) {
                if (start)
                    start = false;
                else
                    out.println("        val = trampoline(val);");
                out.print("        if (val.nil() || ");
                if (bit) out.print("!");
                out.println("val.head()) return null;");
                out.println("        val = val.tail();");
            }
            if (pattern.isLiteral()) {
                out.println("        val = trampoline(val);");
                out.println("        if (!val.nil()) return null;");
            } else if (pattern.isBinding()) {
                out.print("        rt01_.val b");
                out.print(bindingIndex);
                out.println(" = val;");
                bindingIndex++;
            }
        }
        out.print("        return ");
        compileExpr(expr, out);
        out.println(";");
        out.println("    }");
    }

    private void compileExpr(Expr e, PrintWriter out) throws Exception {
        if (e instanceof BoundExpr) {
            out.print("b");
            out.print(((BoundExpr) e).getIndex());
        } else if (e instanceof ConstantExpr) {
            out.print("_");
            out.print(((ConstantExpr) e).getBits());
        } else if (e instanceof ConcatExpr) {
            out.print("new rt01_.concat(");
            compileExpr(((ConcatExpr) e).getFirst(), out);
            out.print(",");
            compileExpr(((ConcatExpr) e).getSecond(), out);
            out.print(")");
        } else if (e instanceof FuncallExpr) {
            out.print("new ");
            out.print(((FuncallExpr) e).getFunction().getMangledName());
            out.print("(");
            boolean start = true;
            for (Expr arg : ((FuncallExpr) e).getArgs()) {
                if (start)
                    start = false;
                else
                    out.print(",");
                compileExpr(arg, out);
            }
            out.print(")");
        }
    }
}
