public class Token {
    public enum Type {
        ZERO, ONE, NIL, DOT, EQUALS, SYMBOL
    }

    private Type type;
    private String symbol;
    private String fileName;
    private int lineNumber;
    private int column;

    public Token(Type type, String symbol, String fileName, int lineNumber, int column) {
        this.type = type;
        this.symbol = symbol;
        this.fileName = fileName;
        this.lineNumber = lineNumber;
        this.column = column;
    }

    public Type getType() {
        return type;
    }

    public String getSymbol() {
        return symbol;
    }

    public String getFileName() {
        return fileName;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public int getColumn() {
        return column;
    }

    public String getLocation() {
        return fileName + ":" + lineNumber + ":" + column;
    }
}
