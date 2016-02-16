import java.io.*; 
import java.net.*;

public class Client {

    public static void main(String [] args) {


	try {

	Socket socket = new Socket("localhost", 8080); 

	BufferedReader r = new BufferedReader( new InputStreamReader( System.in ) ); 
	PrintWriter p = new PrintWriter( new OutputStreamWriter( socket.getOutputStream() ), true ); 


	String l = r.readLine(); 
	while (l != null)
	    {
		p.println(l); 
		l = r.readLine(); 
	    } 

	} catch(IOException e) {
	    System.err.println("ERR: input/output error");
	}

    }

}