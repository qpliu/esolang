package rt01_;

import java.io.OutputStream;
import java.io.PrintStream;

public abstract class function extends val {
    private val val = null;

    protected val trampoline() {
        if (val == null)
            val = eval();
        return val;
    }

    protected abstract val eval();

    public boolean nil() {
        throw new RuntimeException();
    }

    public boolean head() {
        throw new RuntimeException();
    }

    public val tail() {
        throw new RuntimeException();
    }

    public static void main(String[] args, int arity, String name) throws Exception {
        Class<?>[] types = new Class<?>[arity];
        for (int i = 0; i < arity; i++)
            types[i] = val.class;
        int index = 0;
        boolean bits = false;
        if (args.length > 0 && "-bits".equals(args[0])) {
            index = 1;
            bits = true;
        }
        val val = (function) Class.forName(name).getDeclaredConstructor(types).newInstance((Object[]) args(args, arity, index));
        if (bits)
            writeBits(val, System.out);
        else
            write(val, System.out);
        System.out.flush();
    }

    public static val[] args(String[] args, int arity, int index) throws Exception {
        val[] vals = new val[arity];
        val stdin = null;
        for (int i = 0; i < arity; i++) {
            if (index + i < args.length) {
                if (!"-".equals(args[index + i])) {
                    vals[i] = new input(args[index + i]);
                } else {
                    if (stdin == null)
                        stdin = new input(System.in);
                    vals[i] = stdin;
                }
            } else if (stdin == null) {
                stdin = new input(System.in);
                vals[i] = stdin;
            } else {
                vals[i] = NIL;
            }
        }
        return vals;
    }

    public static void writeBits(val val, PrintStream out) throws Exception {
        for (;;) {
            val = trampoline(val);
            if (val.nil())
                break;
            out.print(val.head() ? "1" : "0");
            val = val.tail();
        }
    }

    public static void write(val val, OutputStream out) throws Exception {
        int byt = 0;
        int bit = 128;
        for (;;) {
            val = trampoline(val);
            if (val.nil())
                break;
            byt |= val.head() ? bit : 0;
            bit >>= 1;
            if (bit == 0) {
                out.write(byt);
                byt = 0;
                bit = 128;
            }
            val = val.tail();
        }
    }
}
