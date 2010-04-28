public class ConcatExpr extends Expr {
    private Expr first;
    private Expr second;

    public ConcatExpr(Expr first, Expr second) {
        super(first.getToken());
        this.first = first;
        this.second = second;
    }

    public rt01_.val eval(rt01_.val[] bindings) {
        return new rt01_.concat(first.eval(bindings), second.eval(bindings));
    }

    public Expr getFirst() {
        return first;
    }

    public Expr getSecond() {
        return second;
    }
}
