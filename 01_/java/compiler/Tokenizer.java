import java.io.FileReader;
import java.io.Reader;
import java.util.Iterator;

public class Tokenizer implements Iterator<Token> {
    private Reader in;
    private String fileName;
    private int lineNumber;
    private int column = 0;

    private int pushback = -1;
    private Token next;

    public Tokenizer(String fileName) throws Exception {
        this(new FileReader(fileName), fileName, 1, 0);
    }

    public Tokenizer(Reader in, String fileName, int lineNumber, int column) {
        this.in = in;
        this.fileName = fileName;
        this.lineNumber = lineNumber;
    }

    public boolean hasNext() {
        if (next == null)
            readNext();
        return next != null;
    }

    public Token next() {
        if (next == null)
            readNext();
        Token result =  next;
        next = null;
        return result;
    }

    public void remove() {
    }

    private void pushback(int lastChar) {
        assert pushback < 0;
        pushback = lastChar;
        column--;
        if (lastChar == '\n')
            lineNumber--;
    }

    private int nextChar() {
        int nextChar = -1;
        if (pushback >= 0) {
            nextChar = pushback;
            pushback = -1;
        } else {
            try {
                nextChar = in.read();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        if (nextChar >= 0) {
            column++;
            if (nextChar == '\n') {
                lineNumber++;
                column = 0;
            }
        }
        return nextChar;
    }

    private void readNext() {
        for (;;) {
            int nextChar = nextChar();
            if (nextChar < 0)
                return;
            int saveLineNumber = lineNumber;
            int saveColumn = column;
            switch (nextChar) {
            case '0':
                next = new Token(Token.Type.ZERO, null, fileName, lineNumber, column);
                return;
            case '1':
                next = new Token(Token.Type.ONE, null, fileName, lineNumber, column);
                return;
            case '_':
                next = new Token(Token.Type.NIL, null, fileName, lineNumber, column);
                return;
            case '.':
                next = new Token(Token.Type.DOT, null, fileName, lineNumber, column);
                return;
            case '=':
                nextChar = nextChar();
                if (nextChar != '=') {
                    pushback(nextChar);
                    next = new Token(Token.Type.EQUALS, null, fileName, saveLineNumber, saveColumn);
                    return;
                }
                while (nextChar >= 0 && nextChar != '\n')
                    nextChar = nextChar();
                continue;
            case ' ': case '\t': case '\r': case '\n':
                continue;
            default:
                StringBuilder sb = new StringBuilder();
                sb.append((char) nextChar);
                for (;;) {
                    nextChar = nextChar();
                    if (nextChar < 0) {
                        next = new Token(Token.Type.SYMBOL, sb.toString(), fileName, saveLineNumber, saveColumn);
                        return;
                    }
                    switch (nextChar) {
                    case '0': case '1': case '_': case '.': case '=':
                    case ' ': case '\t': case '\r': case '\n':
                        pushback(nextChar);
                        next = new Token(Token.Type.SYMBOL, sb.toString(), fileName, saveLineNumber, saveColumn);
                        return;
                    default:
                        sb.append((char) nextChar);
                    }
                }
            }
        }
    }
}
