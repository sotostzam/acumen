import java.io.InputStreamReader;
import java.io.FileInputStream;
import acumen.*;
import acumen.Interpreter.*;
import acumen.Parser;
import acumen.Pretty;
import acumen.interpreters.reference.Interpreter;
import scala.collection.immutable.Map;
import scala.Tuple2;
import scala.Option;

class Test {

  static Map<CId,Map<Name,Value<CId>>> transform(final Map<CId,Map<Name,Value<CId>>> state) {
    // write your store manipulation here
    return state;
  }

  public static void  main(String[] args) {
    InputStreamReader in = new InputStreamReader(System.in);
  	Prog ast       = Parser.run(Parser.prog(), in);
    Prog desugared = Desugarer.run(ast);
    Tuple2<Prog, Map<CId,Map<Name,Value<CId>>>> progAndState = Interpreter.init(desugared);
    Prog p = progAndState._1();
    Map<CId,Map<Name,Value<CId>>> state = progAndState._2();

    int i = 0;

    while (true) {
      System.out.println(i + "-----------------------");
      System.out.println(Pretty.pprint(Pretty.prettyStore((Map<CId,Map<Name,Value<CId>>>) state)));
      Option<Map<CId,Map<Name,Value<CId>>>> next = Interpreter.step(p,state);
      if (next.isDefined()) {
        state = transform(next.get());
        i++;
      }
      else break;
    }
  }
}

