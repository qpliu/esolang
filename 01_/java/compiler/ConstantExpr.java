import java.util.List;

public class ConstantExpr extends Expr {
    private String bits;
    private rt01_.constant val;

    public ConstantExpr(Token token, boolean[] bits) {
        super(token);
        StringBuilder sb = new StringBuilder();
        for (boolean bit : bits)
            sb.append(bit ? "1" : "0");
        this.bits = sb.toString();
        val = new rt01_.constant(this.bits);
    }

    public rt01_.val eval(rt01_.val[] bindings) {
        return val;
    }

    public String getBits() {
        return bits;
    }
}
