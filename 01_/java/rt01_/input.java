package rt01_;

import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;

public class input extends val {
    private InputStream in;
    private int byt;
    private int bit;
    private val tail = null;

    public input(String file) throws IOException {
        this(new FileInputStream(file));
    }

    public input(InputStream in) {
        this(in, -1, 0);
    }

    private input(InputStream in, int byt, int bit) {
        this.in = in;
        this.byt = byt;
        this.bit = bit;
    }

    protected val trampoline() {
        if (bit == 0) {
            try {
                byt = in.read();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            if (byt < 0)
                return NIL;
            bit = 128;
        }
        return null;
    }

    public boolean head() {
        return (byt & bit) != 0;
    }

    public val tail() {
        if (tail == null)
            tail = new input(in, byt, bit >> 1);
        return tail;
    }
}
