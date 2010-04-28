import java.io.File;
import java.util.Map;

public class Interpreter {
    public static void main(String[] args) throws Exception {
        Parser parser = new Parser();
        int i;
        String fname = null;
        for (i = 0; i < args.length && !args[i].equals("-"); i++) {
            parser.add(args[i]);
            fname = getFunction(args[i]);
        }
        if (i + 1 < args.length) {
            fname = args[i+1];
            i = i + 2;
        }
        boolean writeBits = false;
        if (i < args.length && "-bits".equals(args[i])) {
            i++;
            writeBits = true;
        }
        Function f = parser.getFunctions().get(fname);
        if (writeBits)
            rt01_.function.writeBits(f.eval(rt01_.function.args(args, f.getArity(), i)), System.out);
        else
            rt01_.function.write(f.eval(rt01_.function.args(args, f.getArity(), i)), System.out);
        System.out.flush();
    }

    private static String getFunction(String fileName) {
        String fn = new File(fileName).getName();
        if (fn.indexOf('.') > 0)
            return fn.substring(0, fn.indexOf('.'));
        else
            return fn;
    }
}
