import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class DefReader implements Iterator<Def> {
    private Iterator<Token> tokenizer;
    private Def def = null;

    public DefReader(Iterator<Token> tokenizer) {
        this.tokenizer = tokenizer;
    }

    public boolean hasNext() {
        if (def == null)
            def = readNext();
        return def != null;
    }

    public Def next() {
        if (def == null)
            return readNext();
        Def result = def;
        def = null;
        return result;
    }

    public void remove() {
    }

    private Def readNext() {
        if (!tokenizer.hasNext())
            return null;
        Token name = tokenizer.next();
        if (name.getType() != Token.Type.SYMBOL)
            throw new RuntimeException(name.getLocation() + ": symbol expected");
        ArrayList<Pattern> patterns = readPatterns(name);
        ArrayList<Token> body = new ArrayList<Token>();
        for (;;) {
            if (!tokenizer.hasNext())
                throw new RuntimeException(name.getLocation() + ": incomplete definition");
            Token token = tokenizer.next();
            if (token.getType() == Token.Type.DOT)
                break;
            body.add(token);
        }
        return new Def(name, patterns.toArray(new Pattern[patterns.size()]), body.toArray(new Token[body.size()]));
    }

    private ArrayList<Pattern> readPatterns(Token name) {
        ArrayList<Pattern> patterns = new ArrayList<Pattern>();
        ArrayList<Boolean> bits = new ArrayList<Boolean>();
        Token startToken = null;
        for (;;) {
            if (!tokenizer.hasNext())
                throw new RuntimeException(name.getLocation() + ": incomplete definition");
            Token token = tokenizer.next();
            if (startToken == null)
                startToken = token;
            switch (token.getType()) {
            case EQUALS:
                if (bits.size() > 0)
                    patterns.add(new Pattern(startToken, toBitArray(bits), null));
                return patterns;
            case ZERO:
                bits.add(false);
                break;
            case ONE:
                bits.add(true);
                break;
            case DOT:
            case NIL:
            case SYMBOL:
                patterns.add(new Pattern(startToken, toBitArray(bits), token));
                bits.clear();
                startToken = null;
                break;
            }
        }
    }

    public static boolean[] toBitArray(List<Boolean> list) {
        boolean[] bits = new boolean[list.size()];
        for (int i = 0; i < list.size(); i++)
            bits[i] = list.get(i);
        return bits;
    }
}
