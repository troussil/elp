import java.io.*; 
import java.net.*;

public class Serveur {

    public static void main(String [] args) {

	int numeroPort = 8080; 

	try {

        ServerSocket connection = new ServerSocket(numeroPort);
	Socket socket = connection.accept();  
	System.out.println("connected to "+socket.getInetAddress()); 

	BufferedReader r = new BufferedReader( new InputStreamReader( socket.getInputStream() ) ); 
	
	String l = r.readLine(); 
	while (l != null)
	    {
		System.out.println(l); 
		l = r.readLine(); 
	    } 

	} catch(IOException e) {
	    System.err.println("ERR: input/output error");
	}

    }

}