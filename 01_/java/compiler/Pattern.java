public class Pattern {
    private Token startToken;
    private boolean[] bits;
    private Token token;
    private String fileName;
    private int lineNumber;

    public Pattern(Token startToken, boolean[] bits, Token token) {
        this.startToken = startToken;
        this.bits = bits;
        this.token = token;
    }

    public Token getStartToken() {
        return startToken;
    }

    public boolean[] getBits() {
        return bits;
    }

    public Token getToken() {
        return token;
    }

    public boolean isLiteral() {
        return token == null || token.getType() == Token.Type.NIL;
    }

    public boolean isWild() {
        return token != null && token.getType() == Token.Type.DOT;
    }

    public boolean isBinding() {
        return token != null && token.getType() == Token.Type.SYMBOL;
    }

    public rt01_.val match(rt01_.val val) {
        for (int i = 0; i < bits.length; i++) {
            val = rt01_.val.trampoline(val);
            if (val.nil() || val.head() != bits[i])
                return null;
            val = val.tail();
        }
        if (isLiteral()) {
            val = rt01_.val.trampoline(val);
            if (!val.nil())
                return null;
        }
        return val;
    }
}
