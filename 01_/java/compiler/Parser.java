import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class Parser {
    private HashMap<String,Integer> arities = new HashMap<String,Integer>();
    private HashMap<String,List<Def>> functions = new HashMap<String,List<Def>>();

    public void add(String fileName) throws Exception {
        add(new Tokenizer(fileName));
    }

    public void add(Reader in, String fileName, int lineNumber, int column) {
        add(new Tokenizer(in, fileName, lineNumber, column));
    }

    public void add(Iterator<Token> tokenizer) {
        addDefs(new DefReader(tokenizer));
    }

    public void addDefs(Iterator<Def> defs) {
        while (defs.hasNext()) {
            Def def = defs.next();
            Token name = def.getName();
            if (arities.containsKey(name.getSymbol())) {
                if (def.getArity() != arities.get(name.getSymbol()))
                    throw new RuntimeException(name.getLocation() + ": arity mismatch in definition of " + name.getSymbol());
            } else {
                arities.put(name.getSymbol(), def.getArity());
                functions.put(name.getSymbol(), new ArrayList<Def>());
            }
            functions.get(name.getSymbol()).add(def);
        }
    }

    public Map<String,Function> getFunctions() {
        HashMap<String,Function> fns = new HashMap<String,Function>();
        for (Map.Entry<String,List<Def>> entry : functions.entrySet()) {
            List<Def> defs = entry.getValue();
            fns.put(entry.getKey(), new Function(defs.toArray(new Def[defs.size()])));
        }
        for (Function function : fns.values())
            function.parse(fns);
        return fns;
    }
}
