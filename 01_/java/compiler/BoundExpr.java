public class BoundExpr extends Expr {
    private int index;

    public BoundExpr(Token token, int index) {
        super(token);
        this.index = index;
    }

    public rt01_.val eval(rt01_.val[] bindings) {
        return bindings[index];
    }

    public int getIndex() {
        return index;
    }
}
