package rt01_;

public abstract class val {
    protected val trampoline() {
        return null;
    }

    public boolean nil() {
        return false;
    }

    public abstract boolean head();
    public abstract val tail();

    public static final val NIL = new val() {
            public boolean nil() {
                return true;
            }

            public boolean head() {
                throw new NullPointerException();
            }

            public val tail() {
                throw new NullPointerException();
            }
        };

    public static val trampoline(val val) {
        for (val v = val.trampoline(); v != null; v = val.trampoline())
            val = v;
        return val;
    }
}
