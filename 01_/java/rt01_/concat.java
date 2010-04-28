package rt01_;

public class concat extends val {
    private val first;
    private val second;
    private val tail = null;

    public concat(val first, val second) {
        this.first = first;
        this.second = second;
    }

    protected val trampoline() {
        first = trampoline(first);
        if (first.nil())
            return second;
        else
            return null;
    }

    public boolean head() {
        return first.head();
    }

    public val tail() {
        if (tail == null)
            tail = new concat(first.tail(), second);
        return tail;
    }
}
