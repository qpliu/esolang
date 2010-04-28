public class FuncallExpr extends Expr {
    private Function function;
    private Expr[] args;

    public FuncallExpr(Token token, Function function, Expr[] args) {
        super(token);
        this.function = function;
        this.args = args;
    }

    public rt01_.val eval(rt01_.val[] bindings) {
        rt01_.val[] argVals = new rt01_.val[args.length];
        for (int i = 0; i < argVals.length; i++)
            argVals[i] = args[i].eval(bindings);
        return function.eval(argVals);
    }

    public Function getFunction() {
        return function;
    }

    public Expr[] getArgs() {
        return args;
    }
}
