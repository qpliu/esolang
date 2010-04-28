package rt01_;

public class constant extends val {
    private String bits;
    private int index;
    private val tail = null;

    public constant(String bits) {
        this(bits, 0);
    }

    private constant(String bits, int index) {
        this.bits = bits;
        this.index = index;
    }

    protected val trampoline() {
        if (index >= bits.length())
            return NIL;
        return null;
    }

    public boolean head() {
        return '1' == bits.charAt(index);
    }

    public val tail() {
        if (tail == null) {
            if (index + 1 < bits.length())
                tail = new constant(bits, index + 1);
            else
                tail = NIL;
        }
        return tail;
    }
}
