import java.io.*;
import java.net.*;

public class EchoClient {
    public static void main(String[] args) throws IOException {

        if (args.length != 2) {
            System.err.println("Usage: java EchoClient <hostname> <port>");
            System.err.println("   Ex: java EchoClient 127.0.0.1 5000");
            System.exit(1);
        }

        Socket echoSocket = null;
        PrintWriter out = null;
        BufferedReader in = null;

        try {
            echoSocket = new Socket(args[0], Integer.parseInt(args[1]));
            out = new PrintWriter(echoSocket.getOutputStream(), true);
            in = new BufferedReader(new InputStreamReader(echoSocket.getInputStream()));
        } catch (NumberFormatException e) {
            System.err.println("port must be an integer");
            System.exit(1);
        } catch (UnknownHostException e) {
            System.err.println("Don't know about host: " + args[0] + ".");
            System.exit(1);
        } catch (IOException e) {
            System.err.println("Couldn't get I/O for the connection to: " + args[0] + ".");
            System.exit(1);
        }

        String line = null;
        while ((line = in.readLine()) != null) {
            System.out.println(line);
            out.println(line);
        }

        out.close();
        in.close();
        echoSocket.close();
    }
}
