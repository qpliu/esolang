public abstract class Expr {
    private Token token;

    protected Expr(Token token) {
        this.token = token;
    }

    public abstract rt01_.val eval(rt01_.val[] bindings);

    public Token getToken() {
        return token;
    }
}
