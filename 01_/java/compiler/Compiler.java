import java.io.FileWriter;
import java.io.PrintWriter;

public class Compiler {
    public static void main(String[] args) throws Exception {
        Parser parser = new Parser();
        for (String arg : args)
            parser.add(arg);
        for (Function function : parser.getFunctions().values()) {
            PrintWriter out = new PrintWriter(new FileWriter(function.getMangledName() + ".java"));
            function.compile(out);
            out.flush();
            out.close();
        }
    }
}
